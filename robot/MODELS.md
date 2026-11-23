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

- **What it predicts.**  From one player's view, for each unseen honor (A, K,
  Q, J, perhaps the 10), the chance that the left-hand hidden hand holds it
  (the right-hand one holds it otherwise), and for each suit the chances of
  each length in the left-hand hand (the right-hand length follows from the
  cards left).  Each seat has its own beliefs, about the two hands that it
  cannot see: declarer about the defenders, a defender about partner and
  declarer.
- **Why it converges to the posterior.**  Every game's deal is known, so each
  play gives exact labels, and it is trained by log loss.  Log loss is a
  proper scoring rule: its minimum is the conditional chance given what the
  model sees, so the training finds the Bayesian answer without our writing
  it down, and a model too small for that finds the closest in relative
  entropy.  The posterior is that of the players in the training games
  (their bidding and their play), as with the dealer's chance that a player
  differs from the robot's call, but measured rather than assumed.
- **Bayes in the wiring.**  In log-odds, Bayes's rule is a sum.  The base is
  vacant places: logit p = log(the unknown cards left of one hidden hand /
  those of the other), which is exact for a random deal, so the model learns
  only the corrections.  Showing out and the cards played are exact zeros,
  wired in rather than learned.  Each play then adds a learned log-likelihood
  ratio: logit p<sub>t</sub> = logit p<sub>t−1</sub> + Δ(the play just seen,
  the public position).  This is a Bayes filter, and the previous beliefs
  are the memory.  To begin with, the previous beliefs are an input computed
  by the current model, with no slope through time; if they are right, Δ
  converges to the true likelihood ratio, so the recursion is consistent.
  The auction's beliefs, before the first play, are its prior.
- **Separate at first.**  It is trained on the games that the player's
  training plays anyway, with weights of its own, so it cannot change the
  player's play or its training.  It is a few dozen outputs.  Before it is
  connected it already serves twice: as a check on the PR 72 dealer, whose
  `read_the_play` estimates the same posterior by weighting deals by the play
  model, by comparing the two calibrations; and as a proposal for the dealer,
  which deals from the beliefs and reweights, for fewer rejections.
- **Joined to the player later.**  A finesse depends on the chance that the
  king is in the right place, and a linear player cannot multiply a belief
  by "this card finesses against the king" unless that product is a
  feature.  So the beliefs join the player as features crossing a belief
  with the card's role, not as beliefs added in.  It is measured like any
  change to the player: matches at equal games.
- **The bidding system.**  The auction's prior depends on the system played
  (Sheets, SAYC, 2/1), so the model must know it: either its inputs are the
  bounds that the system's table gives each call (`shown_bounds`), which
  carry the system with them, or its features are crossed with the system.
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

Comments from Desktop and Pure shaped these checks.

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
