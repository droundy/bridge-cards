# Training the player

A plan for training the play model (`robot/src/ai/fitted_play.rs`): what it
predicts, the objective it is trained by, and why that training should arrive
at the answer.  It follows from the discussion of 2026-11-22 recorded in
[MODELS.md](MODELS.md), and it is written in the language of statistical
mechanics, where it is most natural: tricks are a negative energy, and the
chances of plays are a Boltzmann distribution.

## What the model predicts

For a position and each card that could be played, the model gives a score
s<sub>i</sub> in **tricks**.  The scores mean the differences between the
cards: s<sub>i</sub> − s<sub>j</sub> is how many more tricks card i is worth
than card j.  Each position's scores have a free additive constant (a gauge,
like a chemical potential for each choice), so a score says nothing about
how many tricks the position is worth, only how the cards compare.

The worth v<sub>i</sub> of card i is the mean number of tricks that the side
to play takes from then on, when

- the hands that the player cannot see are dealt by an ensemble that fits
  what the player knows (the auction and the play so far), and
- every later card, by either side, is played as a player at temperature
  T<sub>opp</sub> would play it, choosing card j with the chance
  e<sup>s<sub>j</sub>/T<sub>opp</sub></sup> / Σ<sub>k</sub>
  e<sup>s<sub>k</sub>/T<sub>opp</sub></sup>.

Both sides play on the same way, our own later play included: v is the
worth of a card in a game played by that kind of player, and nothing else.

The chances of the plays are the softmax of the scores at a temperature that
is not part of the model: p<sub>i</sub> ∝ e<sup>s<sub>i</sub>/T</sup>.  The
robot plays the best card (T → 0), self-play and the playouts play at
T<sub>opp</sub>, and reading the opponents uses the T that describes them.
One model serves every temperature, because only the divisor changes.

T<sub>opp</sub> is the one number that the plan leaves to be chosen.  It
should describe interesting opponents, and is probably between 0.1 and 1
trick; it may depend on the stage of the play, which is ignored for now.  It
can be measured: the T that makes a given player's plays most likely, by
maximum likelihood with the model fixed.

## Why chances proportional to e<sup>v/T</sup>

Of all the ways of choosing a card whose mean loss is a given number of
tricks, the one that is least predictable, with the most entropy, gives card
i the chance e<sup>v<sub>i</sub>/T</sup>/Z, where 1/T is the Lagrange
multiplier of the mean loss.  This is the canonical ensemble with energy
−v, and in game theory it is the logit quantal response.  It makes "close to
the best" exact: a card g tricks worse than the best is played
e<sup>−g/T</sup> times as often.  Equal cards have equal chances, which the
player must give them if it is to be used to read opponents: a model that
gives one of two equal cards all of the chance takes the other for a
mistake that says something about the hand.

The entropy is counted over distinct plays.  The cards of a run of touching
cards win the same tricks, so a run is one play, and its chance is shared
equally among its cards.  That is the principle of restricted choice.

## The objective

The model is trained to maximize, over the positions that self-play reaches,
the free energy at a temperature T (with energy −v it is −F = −⟨E⟩ + TS):

J<sub>T</sub> = Σ<sub>i</sub> p<sub>i</sub> v<sub>i</sub> + T H(p),  H(p) =
−Σ<sub>i</sub> p<sub>i</sub> log p<sub>i</sub>

with p the model's chances at T.  The slope of J<sub>T</sub> with card i's
score is

∂J<sub>T</sub>/∂s<sub>i</sub> = p<sub>i</sub> (v<sub>i</sub> −
Σ<sub>j</sub> p<sub>j</sub> v<sub>j</sub> − T(log p<sub>i</sub> −
Σ<sub>j</sub> p<sub>j</sub> log p<sub>j</sub>)) / T.

Each position's slope is passed back through the model's tables and network
like any other, and the model's weights are moved up it.

The worth v<sub>i</sub> is not known; a playout gives an estimate, the mean
over a few deals of the ensemble, or the tricks on the one deal that was
really dealt.  The slope is linear in v, so an estimate whose mean is v
gives a slope whose mean is the true slope: the noise of the playouts
averages away, and does not bias the answer.  That would not be so of a fit
to target chances e<sup>v/T</sup>/Z, whose exponential turns noise in v into
a bias.

## Why the training should arrive at the answer

**The fixed point.**  The slope is zero for every card exactly when
p<sub>i</sub> ∝ e<sup>v<sub>i</sub>/T</sup>, that is when s<sub>i</sub> =
v<sub>i</sub> + c for the position's constant c.  This is so at every T: the
fixed point does not depend on the temperature of the training, only on the
definition of v.  If the model can represent v, the free energy has this one
maximum, and gradient ascent with noisy but unbiased slopes and a small
enough rate (stochastic approximation) arrives at it.

**A small model cannot represent v.**  Its tables describe cards, not every
detail of a deal, so it can only approximate the worth of each card, and the
objective then says where its errors go.  In J<sub>T</sub> an error in card
i's score costs in proportion to p<sub>i</sub>, the chance that the card is
played: an error on a card that no one would play costs no tricks, and an
error on the card that will be played, or on one close to it, costs most.
So the free energy at the temperature of use puts the model's limited
accuracy where play happens.  This is the reason for the weighting, and the
evidence for it is what happened without it: fitting every card's worth
alike by least squares, the level of each position included, made the
learners worse at choosing.  They spent their capacity on cards that are
never played and on the level, which a choice does not depend on, and the
likeliest card's loss to the best played out rose from 0.086 to 0.11 tricks
a play while the fit of the level improved.  The scores here have no level
to fit.

**Local maxima, and annealing.**  With an inadequate model the free energy
has local maxima, and fitting well only the cards that it plays is
self-reinforcing.  A model sure of a wrong card learns that a better one
exists only at a rate proportional to the better card's chance, which may be
10<sup>−4</sup>.  That is the stall of the early self-play, when the entropy
fell too low and the learners stopped learning: a local maximum of the right
objective, not a wrong objective.  The remedy is deterministic annealing.
Maximize J<sub>T</sub> first at a high T, about 1 trick, where it is smooth
and every card is weighted and learned, then lower T slowly, geometrically,
to T<sub>opp</sub>, following the maximum down.  The temperature of the
weights and that of the entropy are lowered together, so that each step is
the slope of J at its T.  Since the fixed point is the same at every T, the
schedule does not change the answer; it only keeps the training from being
caught on the way.  Training ends at T<sub>opp</sub>, the hottest temperature
that the model will be used at: going colder afterwards only sharpens the
chances among the cards that it learned well, where going hotter would give
weight to cards that it learned badly.

**Weights that the training may not use.**  A card's weight may not depend
on the same playouts as its target, as in "the model's chance plus the
playout's chance": a weight that rises when the sampled worth happens to be
high biases the fit toward lucky noise.  Weights from an independent half of
the deals are unbiased (each half weighted by the other, so that none are
wasted), but they make the slope a weighted regression rather than the slope
of J, and a single true deal has no half to spare, so they are an option and
not the design.

## How the training runs

- **Games.**  Self-play deals, bid by the robot, are played by the learners
  and past models with every card chosen at T<sub>opp</sub>.  They set which
  positions are trained.
- **Played-out choices.**  After each game a few of its choices (those with
  more than one run of touching cards) are chosen at random and played out,
  each weighted by the number of choices over the number chosen, so that the
  positions are those that the games reach.  The share played out could
  later be set for each kind of situation, to train the rare ones more often,
  and then weighted by one over its share, which is importance sampling and
  keeps the objective the same.
- **Every card is valued.**  At a played-out choice each run is played out,
  on the same deals, and each card takes its run's worth.  Valuing every card
  costs more than playing one, but it is what makes the slope exact.
- **The deals.**  Either the true deal (an unbiased draw of the hidden hands,
  noisy) or an ensemble from the dealer (`Hidden::read_the_play`, less noisy,
  as accurate as the dealer).  The plan works with either, and the two
  sessions run one each, which measures whether the dealer's bias or the true
  deal's noise matters more.
- **Common random numbers.**  The later plays of a playout are sampled at
  T<sub>opp</sub>.  To keep the differences between cards sharp, one uniform
  number u is drawn for each later trick and seat, and is shared by the
  playouts of every card of the choice.  It is turned into a card by sorting
  the cards that could be played by their chance, best first, and taking the
  card at u in the cumulative chances, so that u is the quality of the play:
  the same u is the same degree of error on every playout, even where their
  positions differ.  Ties are broken by the card.
- **Units.**  The learners of round 88000 were trained at temperature 1 in
  their own units.  Multiplying all of their scores by T<sub>opp</sub> puts
  them in tricks with no change to their play at T<sub>opp</sub>
  (`Network::scale_scores`), and no other calibration is needed.
- **The schedule.**  T from 1 trick down to T<sub>opp</sub> geometrically,
  over a number of rounds to be found; the rate of Adam constant, or lowered
  at the end.

## Measuring it

- **The zero line.**  The untrained learners of round 88000 against the
  reference (`round-88000.bin#4`), before anything else, so that every run is
  judged by how far it moves from where it began.
- **Matches.**  Against the reference and the robot, at equal games played,
  as before.  These say whether the play is better.
- **The free energy.**  The mean of J at T<sub>opp</sub> over the positions
  played out, as it is trained, and the loss of the likeliest card to the
  best played out.  These say whether the training does what it is meant to.
- **The temperature.**  The T that makes the learners' own plays most likely
  against 100-deal values, by stage (leads, following, discards) and by
  side, to see whether one T describes them.

## Then

Once the training holds or improves the play:

1. The systematic study of tables and shapes, which was set aside.
2. Separate weights for declarer and for defence: in effect one model with
   every feature crossed with a "declarer" feature.
3. Choosing T<sub>opp</sub>, and whether it depends on the stage.
