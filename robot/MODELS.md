# A catalog of play models

What the robot might learn, what each model is for, and how it could be
trained.  It is a plan, not a description of the code: only the player
exists today (`robot/src/ai/fitted_play.rs`).

## The player comes first

The player chooses a card.  Its scores mean nothing but their order, which is
why it could be trained well by self-play, and it is what the browser plays
by.  The work it needs next is the systematic exploration of tables and
shapes that was set aside for predicting tricks.  A good player is also what
the other models are trained with: playouts and ensembles of reasonable play
need reasonable players.

What the player still lacks is a principled meaning for its chances (see
[Chances of plays](#chances-of-plays)).  It does not need them to play, but
reading the opponents does.

## Models, by what their output means

What the output must mean decides what the model is trained on.

| Kind | The output says | Used by | Trained on |
|---|---|---|---|
| Player | only the order of the cards | the robot's play (built) | self-play, or the expected tricks of its play |
| Chances | how likely a player is to play each card | reading the opponents: `play_likelihood` in the dealer | plays, with chances defined as below |
| Card values | each card's tricks relative to the best | ordering and pruning a search: every play within ½ a trick of the best | ensemble or playout values, differences only |
| Position value | the tricks from here, ideally their spread | search leaves; bidding (the chance of game or slam) | double-dummy results or playouts, level included |
| Card locations | for each unseen honor, the chance that each hidden hand holds it, and the chances of each suit's length | the player, as beliefs it remembers; dealing ensembles, or narrowing the dealer | the true deal of each game, by log loss (see [Card locations](#card-locations)) |

Chances and card locations are two sides of reading the opponents: one
scores sampled deals by the plays that they explain, the other deals them.

## Models, by what they know

What a model knows decides how it can be labelled.

- **The whole deal**, one sampled deal or the real one.  The double-dummy
  solver labels it exactly, with no playout noise and no dealer.  Averaged
  over deals that fit what is known, a whole-deal value is the usual estimate
  of a contract for bidding.  It assumes that everyone plays as though all
  the cards were in sight, which flatters whoever gains from that, so it is
  to be checked against playouts before it is trusted at search leaves.
- **One player's view**: their hand, the dummy, the auction and the play.
  This is what choosing a card needs.  Its labels are averages over the
  hidden hands (an ensemble from the dealer) or self-play results, and are
  noisy.
- **The public view**: the auction and the dummy.  It is what partner and the
  opponents can infer, and all that the opening leader has besides their hand.
- **Before the play**: a hand and the auction, or the ranges that partner has
  shown.  This is the bidding question, and its answer is a spread of tricks,
  since game or slam depends on a count and not on a mean.

## Models, by role

- **Opening lead**: 13 cards and the auction.  It has the least information
  and the most loss: about half of the tricks that the playout yardstick
  says the player loses are leads.  It has its own design today
  (`likely_opening_leads`) and probably deserves its own shape, or a
  whole-deal value averaged over deals of the three hidden hands.
- **Declarer**: sees 26 cards, plays both hands, and can plan to the end.
  This is nearly a one-player planning problem, where search over sampled
  defenders with a value model at the leaves should do better than a learned
  policy alone.
- **Defender**: sees 26 cards but plays with a partner that it cannot see,
  so signals, reading partner and a shared plan matter.  A learned policy,
  with chances and card locations, matters most here.

The tables already describe the role, so a cheap first step is the same
shape with separate weights for each role, at the cost of each seeing less
data.

## What could be shared

- **Card tables** (rank in the suit, sequences, who holds the cards above,
  voids, place in the trick) serve every model that scores cards: the
  player, chances and card values.
- **Position features** are needed by value models and would serve bidding
  too: the top cards left in each suit, lengths, trump lengths, points, the
  tricks left, and the ranges that the auction showed.  Today's tables have
  none of these.

## What we would build with them

| Thing | Needs |
|---|---|
| The browser player | the player, fast |
| A guided or recursive search | card values to order and prune, position values at the leaves |
| The dealer and deal reader | chances calibrated to the players being read (our learners in self-play; human games for people), or card locations |
| Bidding | position values: whole-deal ones averaged over deals that fit the auction, or from the public view |
| Training | a position value as a baseline: subtracting it from each playout result lowers the noise without biasing the slope |
| Yardsticks | to measure the others |

## Chances of plays

The player can give one of two equivalent cards all of its chance and rate
the other as low as it likes without playing any worse.  A model that reads
the opponents, though, must know that either card was as likely, or it will
take an equal card for a mistake that says something about the hand.  So the
chances need a definition of their own.

One principled definition: of all the ways to choose that lose no more than
a given amount on average, the one that is least predictable (has the most
entropy) gives each card a chance proportional to e<sup>v/T</sup>, where v
is the card's worth in tricks and T is set by the amount allowed.  A policy
trained to maximize its expected tricks plus T times its entropy converges
to it, and its scores become the cards' worth in units of T, up to a
constant: equal cards get equal chances because they are worth the same,
and the scores are card values as well.  What is chosen is then a loss
tolerance in tricks, which means something, rather than a temperature or an
entropy target.  For reading real opponents the temperature would instead be
fitted, by maximum likelihood, to the plays of the players being read.

This is the logit quantal response of game theory, and the policy of
entropy-regularized ("soft") reinforcement learning.  It makes "close"
exact: a card g tricks worse than the best is played e<sup>−g/T</sup> times
as often.  At T = 0.1 tricks a card half a trick worse is played about 0.7%
as often as the best, and one 0.05 worse about 60% as often.

- **One bound on the mean loss, so one T.**  A choice with many cards near
  the best then gives up a little more than one with a clear best, which is
  the natural reading; a bound for each choice, with a T for each, would be
  a different model, and the two should not be mixed.
- **Entropy over distinct plays.**  The cards of a run of touching cards win
  the same tricks, so the entropy is counted over runs, and each run's
  chance is shared equally among its cards.  Counted over cards, a run of k
  would get k times the chance of a single card against the other plays,
  and the old entropy target of self-play was met mostly that way, by
  equivalent cards, while cards close to the best but different stayed
  below 1e-4.  Shared equally within a run, the chances are the principle
  of restricted choice, so a model of a player who chooses at random among
  equal cards reads them as a bridge player does.  Signals break that
  equality on purpose, and a policy whose values include partner's later
  inference can learn them; the definition still holds.
- **The slope.**  Card i's score rises by p<sub>i</sub>(v<sub>i</sub> −
  Σ<sub>j</sub> p<sub>j</sub>v<sub>j</sub> − T(log p<sub>i</sub> −
  Σ<sub>j</sub> p<sub>j</sub> log p<sub>j</sub>))/T, which is unbiased when
  v is a noisy playout result and is zero at p ∝ e<sup>v/T</sup>.  The
  scores then become v/T up to a constant: the player also gives card values
  near the best, and chances, if T is fitted to the players being read.
- **Soft values.**  Exactly, v is a soft value: the tricks plus T times the
  entropy of the choices after, played by the same policy.  The plan
  ([PLAYER_TRAINING.md](PLAYER_TRAINING.md)) plays everything after the
  trained choice at T<sub>opp</sub> by both sides, and counts only the tricks.
  That gives the policy for one choice at a time, which is close to the
  exact one when T is small, and is the place to start.
- **The tolerance.**  It is the one thing chosen, in tricks a play.  A
  natural choice is how accurate the players actually are: the mean loss a
  play of the learners on the playout yardstick (about 0.08 tricks now).

## Card locations

The user's idea (2026-11-23): give the player beliefs about where the hidden
cards are, as chances that it carries from one play to the next, so that it
has a memory, with the Bayesian reasoning wired in.  `GameState` keeps no
play history, so today the player sees only what is left.

- **What it predicts.**  From one player's view, the beliefs are a
  distribution over the ways that the unseen cards can be split between the
  two hidden hands.  Each seat has its own, about the two hands that it
  cannot see: declarer about the defenders, a defender about partner and
  declarer.  With x<sub>c</sub> = 1 when card c is in the left-hand hand and
  L<sub>s</sub> that hand's length in suit s,

  P(x) ∝ exp(Σ<sub>c</sub> h<sub>c</sub>x<sub>c</sub> +
  Σ<sub>s</sub> φ<sub>s</sub>(L<sub>s</sub>)),

  over the splits that give each hand the number of cards that it has left.
  The outputs are a field h<sub>c</sub> for each unseen card (the spots too,
  by features shared across ranks, since the 9 and the 8 decide some
  finesses) and a potential φ<sub>s</sub> for each suit and length.  The
  chance of each honor and the chances of each suit's length are both read
  from it exactly: each suit's sum over its own splits is a polynomial of
  degree at most 13 in one variable that counts the left hand's cards, and
  the coefficient of the left hand's count in the product of the four gives
  the normalization, with the chances from the same sums, in microseconds.
- **One model for lengths and honors.**  These statistics make the family the
  maximum-entropy distribution with the given honor chances and length
  distributions, so the two kinds of belief cannot contradict each other (as
  separate outputs could, when a suit's expected length is not the sum of
  its cards' chances), and a deal can be dealt from them.  The correlation
  between length and honors is there twice: a suit's length counts its
  honors, and a potential that favours length raises every card of the suit
  together.  A potential on the left hand's points, ψ(HCP), fits the same
  way (a second variable in the polynomials, 0 to 37), and carries what the
  auction showed, such as 15–17: the bounds of `shown_bounds` become
  potentials that are learned rather than walls.
- **Why it converges to the posterior.**  Every game's deal is known, so each
  play gives exact labels, and it is trained by log loss.  Log loss is a
  proper scoring rule: its minimum is the conditional chance given what the
  model sees, so the training finds the Bayesian answer without our writing
  it down, and a model too small for that finds the closest in relative
  entropy.  The posterior is that of the players in the training games
  (their bidding and their play), as with the dealer's chance that a player
  differs from the robot's call, but measured rather than assumed.
- **Convex, by moment matching.**  The log loss is minus the log likelihood
  of the true split, and for a family of this kind it is convex in h, φ and
  ψ, with a slope that is the observed statistics less
  their expectations (is each card on the left, is each suit this long):
  moment matching.  With h, φ and ψ linear in features, the whole fit is
  convex, with one optimum and no annealing.  At that optimum the honor
  chances and length distributions match the posterior's, as far as the
  features can tell situations apart, and the rest of the joint is the
  maximum-entropy one with them.
- **Bayes in the wiring.**  In the natural parameters h, φ and ψ, Bayes's
  rule is a sum: each play adds the log-likelihood ratio of the play, as a
  function of the split, projected onto the family: h<sub>t</sub> =
  h<sub>t−1</sub> + Δh(the play just seen, the public position), and so for
  φ and ψ.  This is a Bayes filter, and the previous parameters are the
  memory.  Vacant places needs no base term: with every field and potential
  zero, the fixed counts make every split equally likely, which is vacant
  places, and the counts follow the cards as they are played.  A played card
  leaves the sums and showing out fixes a suit's length, both exactly.
  (The chances themselves are not additive in log-odds across plays,
  because the counts couple the cards: a card played by the left hand lowers
  every other card's chance to be there.  A Δ added to marginal log-odds
  would need that base recomputed after every play and would count it twice
  otherwise.)  To begin with, the previous parameters are an input computed
  by the current model, with no slope through time; if they are right, Δ
  converges to the projected likelihood ratio, so the recursion is
  consistent.  Projecting each play's evidence onto the family is
  assumed-density filtering, and the error of each projection carries into
  the later beliefs; the enumerated endings below measure it.  The parameters
  before the first play are the prior, from the auction (below).
- **Separate at first.**  It is trained on the games that the player's
  training plays anyway, with weights of its own, so it cannot change the
  player's play or its training.  It is about 130 outputs (up to 39 fields, 56
  length potentials and 38 for points).  Before it is
  connected it already serves twice: as a check on the PR 72 dealer, whose
  `read_the_play` estimates the same posterior by weighting deals by the play
  model, by comparing the two calibrations; and as a proposal for the dealer.
  The family can be dealt from exactly, suit by suit (each suit's length
  from the polynomials, then its cards given the length), with the exact
  chance of each deal dealt, so the dealer's reweighting is exact and has
  fewer rejections.
- **One shape all through the play (the user's requirement).**  The beliefs
  cover every card, seen or unseen, played or not, so that their shape never
  changes during the play.  Each card's belief is its chances of being in
  each place relative to the player: their own hand, partner's, the
  left-hand opponent's, the right-hand one's, or already played.  A card in
  sight has chance 1 in its place, which is what the player's tables know of
  it today, so the beliefs have the shape of those "known" inputs, and an
  unseen card's chances replace the 0 or 1.  The family's fields are indexed
  the same way, by all the cards, with those in sight or played held fixed
  rather than fitted.
- **Places by seat, shown by the player.**  The beliefs belong to what a
  side can see, so they are kept by seat and turned into places relative to
  the player only when the player reads them.  Declarer sees the same cards
  whichever hand it plays from, but its left-hand opponent changes when it
  plays from the dummy, so beliefs kept as "left" and "right" would swap
  sides from one trick to the next.  The two hidden hands of the family are
  the first and second hidden hands after the player, as the tables'
  `FirstLength` and `SecondLength` already have them: for declarer the
  left-hand and right-hand opponents, for the defender with the dummy on
  their left partner and then the right-hand opponent (the declarer), and
  for the one with the dummy on their right the left-hand opponent (the
  declarer) and then partner.
- **Joined to the player later.**  A finesse depends on the chance that the
  king is in the right place, and a linear player cannot multiply a belief
  by "this card finesses against the king" unless that product is a
  feature.  So the beliefs join the player through the tables' existing
  features about who holds which card (such as who holds the cards above
  this one), with chances where today there are certainties, rather than as
  beliefs added in beside them.  The principled form of "chances in place of
  certainties" is the expected score: each table entry weighted by the
  chance of the situation it stands for, which is exact for a linear player.
  `Above` and `Below` are entries for the holders of three cards together,
  so they need the joint chances of those three, which the family gives
  exactly, and not the product of each card's chance: in a suit where the
  first hidden hand holds exactly one of two unseen cards, each card has
  chance ½ there, but both are never there together.  Splitting "hidden"
  into the two hidden hands gives each of the three cards five states rather
  than four, 125 entries a place, which is the size that `Above1` and
  `Below1` already have (their fourth hand is never used today).  If both
  hidden hands start with today's weight for "hidden", the expected score is
  today's score until training moves them apart, so the join starts from the
  player as it is.  The length and points tables, which today take the
  bounds that the auction showed, would instead take the expected weight
  over the belief's length and points distributions.  It is measured like
  any change to the player: matches at equal games.
- **The auction enters only through what it shows (the user's rule).**
  Nothing in the model depends on the bidding system, and no actual bid is
  ever an input.  The auction reaches the beliefs only through what the
  system says the calls showed, and only as the prior at the start of the
  play: the parameters h, φ and ψ before the first card are fitted, by the
  same moment matching, to an ensemble dealt to agree with the auction (the
  PR 72 dealer's posterior), which makes them the maximum-entropy beliefs
  with that ensemble's card chances, lengths and points; or they hold the
  ranges that `shown_bounds` gives each call as constraints.  Either way the
  system does its work outside the model, and what the model learns, the
  update Δ from each play, is the same whatever system was bid.  This holds
  for the player as well: its `FirstLength`, `FirstPoints` and the like
  already read what the auction showed (`Shown`) and not the calls, while
  the shape `v2-bids` reads who bid each suit (`Table::Bids`), which this
  rule rules out (none of the four shapes being trained uses it).  So
  does the fitted opening lead (`likely_opening_leads`, with its
  `NOTRUMP_WEIGHTS` and `SUIT_WEIGHTS`), whose features are built from the
  calls: partner or the dummy bid the suit, a double, partner's double of an
  opponent's bid, trumps with an opponent bidding another suit.  `EnsemblePlay`
  uses it to choose which leads to solve.  Its features should come from
  `Shown` or the dealt ensemble instead.

  Two things are not calls in this sense.  The final contract and who
  declares are rules of the game, and the player must read them (the
  `Contract` and `Trumps` tables).  The contract is also evidence about the
  hands (who reaches 3NT tends to hold stoppers), and how much depends on how
  the training players bid; the rule cannot remove that.  Training mixes the
  systems, so the learned weights average that evidence over them, and the
  prior from the ensemble is what carries each system's part.  A belief
  model trained on the mixture is calibrated for the mixture, so its
  calibration is checked for each system as well as at each stage.
  Reading the calls to deal the ensemble (`Hidden`, `replaying_the_auction`)
  is the use the rule allows.
- **How it is judged.**  By log loss against two baselines: vacant places,
  and the auction alone (deals dealt uniformly within the bounds that the
  tables give each call), which says how much the learned corrections add to
  what the tables already know.  By calibration (of the cards given a chance
  near p, a fraction p in the left hand), overall and at each stage of the
  play (after 1, 5 and 9 tricks), since with no slope through time an early
  error carries into the later beliefs.  And the recursion alone, on games
  whose exact posterior can be counted: deals dealt uniformly with no
  auction, played by a known random policy, with few enough cards left to
  enumerate.  The play is informative even then (which card of a suit a
  random player follows with depends on its length), so the check is
  against the enumeration and not against vacant places.

Desktop suggested the second baseline, the system, the stages and the enumeration.

## Order of work

1. The player: its objective, and the systematic study of tables and shapes.
2. Card locations, trained alongside the player from the same games but kept
   apart from it, once the player's training has been checked; then joined
   to it.
3. Chances, defined as above, so that the player can read the opponents.
4. A whole-deal position value from double-dummy labels: it serves bidding,
   search leaves and, as a baseline, training.
5. Card values and a guided search for declarer.
6. Shapes for each role, starting with the opening lead.
