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
| Card locations | for each unseen card, the chance that each hidden hand holds it | dealing ensembles directly, or narrowing the dealer | deals, given the auction and the play |

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

## Order of work

1. The player: its objective, and the systematic study of tables and shapes.
2. Chances, defined as above, so that the player can read the opponents.
3. A whole-deal position value from double-dummy labels: it serves bidding,
   search leaves and, as a baseline, training.
4. Card values and a guided search for declarer.
5. Card locations, and shapes for each role, starting with the opening lead.
