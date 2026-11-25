# The play model's tables and shapes

A guide to what the learned player looks at, as of 2026-11-24.  The code
is `robot/src/ai/fitted_play.rs`: `Table` for the tables, `Shape` for
the shapes.  [MODELS.md](MODELS.md) is the plan for other models; this
file is about the player that exists.

## How the player sees a card

For each card it could play, the player looks up **one entry in each
table**: a table is a question about the card and the moment ("is this
card higher than everything left in its suit?"), and its entries are the
possible answers.  Each entry has a weight that is added to the card's
score, and it also feeds the **hidden layers** (16 units, then 8), which
let the model weigh combinations of answers from different tables.  The
card with the highest score is played.  Nothing in the tables says which
card is good; that is all in the learned weights.

Most tables are split by a few words that recur:

- **Role**: declarer playing their own hand, declarer playing the
  dummy's, or the defender on declarer's left or right (4).
- **Place**: how many cards are already in the trick (0–3).
- **How**: whether the card leads, follows suit, ruffs, or is discarded.
- **Play**: the place and how together.  There are 10: a lead, or a card
  that follows, ruffs or is discarded in second, third or fourth place.

**The rule about the auction**: no table reads the calls themselves.
The auction comes in only through what the bidding system's tables say
the calls showed: the fewest and most cards of each suit and points each
hidden hand can hold (`Shown`), narrowed by the play.

## The tables

### The base tables (shape `v2`, 2,900 entries)

| Table | Entries | What it says |
|---|---|---|
| `Trick` | 160 | role, play, whether partner is winning the trick, whether this card beats the trick so far |
| `Rank` | 120 | play, how many cards of the suit are left above this one (0–5+), whether it is the player's lowest of the suit |
| `Above` | 256 | place, and who holds each of the 3 cards just above this one: the player, the other hand in sight, hidden, or played already |
| `Below` | 256 | the same for the 3 cards just below |
| `Lengths` | 128 | place, the player's length in the suit, the other visible hand's length, whether it is trumps |
| `TricksLeft` | 60 | play, and how many tricks are left (6 bands) |
| `Contract` | 240 | role, play, and how declarer's tricks still needed compare with the tricks left: made, beaten, or 0, 1, 2 or more to spare |
| `FirstLength` | 200 | play, and the fewest and most cards of this suit that the first hidden hand after the player can hold, by the auction and the play |
| `SecondLength` | 200 | the same for the second hidden hand |
| `FirstPoints` | 640 | role, place, the fewest and most points the first hidden hand can still hold, and whether the card is an honour |
| `SecondPoints` | 640 | the same for the second hidden hand |

`Above` and `Below` are the closest the model comes to knowing about
sequences, finesses and tenaces: whether the card is touching a higher
one, or sits under a hidden honour.

### Tables added and screened

| Table | Entries | What it says | Result |
|---|---|---|---|
| `Trumps` | 1,280 | play, how many trumps the player has (0–3+), how many the other visible hand has, how many are hidden (0, 1–2, 3–4, 5+), whether the contract is notrump: drawing trumps, ruffing and overruffing | helped; in every shape since |
| `History` | 800 | role, play, who first led this suit this hand (nobody, the player, or which other seat) and in how many tricks it has been led: returning partner's suit, not helping declarer's, seeing a ruff coming | **+0.041 ± 0.008** tricks a deal on top of `Trumps`; in the best shape |
| `Later` | 360 | role, play; whether the other visible hand plays after the player and could beat the trick; whether a hidden hand plays after and is known void in the suit led | helped on top of plain `v2`, but **+0.008 ± 0.012** (zero) on top of `Trumps` and `History` |
| `Later2` | 640 | role, play; for the hidden hands still to play in the trick, separately for opponents and partner: not after me, can't beat, could beat by following, could beat by ruffing ("could" by the cards unseen, the suits shown out of, and the lengths the auction showed) | **+0.023 ± 0.007** on top of `Trumps` and `History`, all four runs positive; in `v2-big` |
| `Bids` | 640 | role, play, and which seats bid this card's suit | **not used**: it reads the calls, which the rule forbids |

### Tables that use card-location beliefs (branch `join-beliefs`)

These read Pure's card-location model: for every unseen card, the chance
that each hidden hand holds it, learned from the auction's bounds and from
how the hidden hands have played (including, for a defender, declarer's
plays from dummy).  The chances are frozen while the player trains, and
each costs about 4× in training time.

| Table | Entries | What it says | Result |
|---|---|---|---|
| `AboveSplit` / `BelowSplit` | 500 each | `Above`/`Below` with "hidden" split into "first hidden hand" and "second", each way weighted by its chance | **+0.008 ± 0.005** (zero) after 10,000 rounds; Pure's two runs still to come |
| `Later3` | 640 | `Later2` with "could" replaced by the chance that the hand does: each of the 16 joint states weighted by its exact probability | built; to be screened against `Later2`, now that `Later2` paid |
| `Later3Knots` | 1,280 | `Later3` with each chance on a scale that can bend at ½ (a tent at ½ and a ramp to 1) | built; tested against `Later3` only if `Later3` pays |

Setting the beliefs to the true deal (`PLAY_SELF_ORACLE=1`) gives the
ceiling of all of these: what perfect knowledge of the hidden cards would
be worth to the network.  Pure is running that test now.

### Legacy tables

`Trick1`, `Above1`, `Below1`, `Rank1`, `TricksLeft1`, `Contract1`,
`FirstLength1` and `SecondLength1` are the tables of the first shape,
`v1`, with room for combinations that cannot happen.  A `v1` model is laid
out as `v2` before it grows; nothing trains in them now.

## The shapes

A shape is a list of tables and the sizes of the hidden layers.  A model
can **grow** into a shape with more tables or wider or deeper layers and
still play exactly as it did: the new entries and units start at zero, and
an added layer starts by passing the last one through unchanged.  So every
experiment starts from model #3 and not from scratch.

| Shape | Tables | Layers | Status |
|---|---|---|---|
| `v2` | base | 16, 8 | the base |
| `v2-trumps` | base + `Trumps` | 16, 8 | led the early screens |
| `v2-trumps-history` | base + `Trumps` + `History` | 16, 8 | **the best so far** |
| `v2-trumps-wide`, `v2-trumps-history-wide` | as above | 32, 16 | width didn't help (−0.02) |
| `v2-trumps-history-deep` | base + `Trumps` + `History` | 16, 8, 8 | +0.011 ± 0.008 at 10,000 rounds; may need longer |
| `v2-later-trumps-history` | + `Later` | 16, 8 | `Later` adds nothing here |
| `v2-trumps-history-later2` | + `Later2` | 16, 8 | +0.023 ± 0.007 over `v2-trumps-history` |
| `v2-big` | base + `Later` + `Trumps` + `History` + `Later2` | 64, 32, 16 | the long run, starting soon, grown from the best averaged model |
| `v2-trumps-beliefs` (on `join-beliefs`) | `Trumps` shape with `AboveSplit`/`BelowSplit` | 16, 8 | zero so far |
| `v2-later`, `v2-history`, `v2-later-history`, `v2-later-trumps`, `v2-bids` | earlier screens | 16, 8 | superseded |

## How good it is now

Measured in tricks a deal against the old published model #4, on 2000
fixed deals, relative to how model #3 does against #4:

| | Tricks a deal better than #3 |
|---|---|
| `v2-trumps-history`, 10,000 rounds | +0.149 |
| 30,000 rounds | +0.203 |
| 60,000 rounds | +0.240 (still rising, but slowly) |
| the average of four learners' weights at 60,000 | about +0.25 |

Each number uses the learner's weights averaged over its last 1,000
rounds, which removes the noise of training at a constant step.
