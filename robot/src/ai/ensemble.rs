//! Choosing a card by its double-dummy value over an ensemble of the hands that
//! cannot be seen.
//!
//! A player sees their own hand and the dummy, and does not see the other two.
//! What they do know is how many cards each of those has, which suits each has
//! shown out of by not following, which cards have been played, and what the
//! auction said about each hand.  [`Hidden`] deals the cards that are not in
//! sight into the two hands again and again, keeping only the deals that agree
//! with all of that, which is an ensemble of the hands that might be there.
//! [`EnsemblePlay`] works out, for each card that it may play, how many tricks
//! it wins with best play by everyone in every deal of the ensemble, and plays
//! the card whose mean is best.
//!
//! The bounds that the auction shows are only the extremes of each hand: a hand
//! of 14 points with five spades fits an opening of 1♠, and so does one that
//! would have opened 1♥.  So the ensemble can also be narrowed to the deals
//! that the robot would have bid the auction with, by bidding each of them
//! again ([`Hidden::closest_to_the_auction`]).  People do not always bid as the
//! robot does, so it keeps the deals that make every call and then those that
//! miss fewest, as many as it wants.
//!
//! Two things in this are not true to the game.  Best play by everyone with
//! every hand known is not how anyone plays, since a defender who cannot see
//! the declarer's hand would not play as though they could.  And the ensemble
//! gives every deal that it keeps the same weight, as though each hand were as
//! likely as any other that would have been bid the same way.

use super::{CallMatcher, Convention, PlayAI, RandomPlay};
use crate::{Bid, BiddingSystem, GameState, Seat};
use bridge_deck::{Card, Cards, HandValuation, Suit};
use bridge_solver::{Naive, Starting};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

/// How many deals to try for one that fits the auction before taking one that
/// does not.
const ATTEMPTS: usize = 1000;

/// How many deals are sampled for each one wanted in an ensemble that is to be
/// bid as the auction was, since many of them are not.
pub(crate) const OVERSAMPLE: usize = 4;

/// The two hands that a player cannot see, and what they know of them.
#[derive(Debug)]
pub(crate) struct Hidden {
    /// The seats of the hands.
    seats: [Seat; 2],
    /// How many cards each hand has now.
    sizes: [usize; 2],
    /// The cards that the two hands hold between them.
    pool: Cards,
    /// The cards that each hand cannot hold, since it has shown out of their
    /// suits.
    forbidden: [Cards; 2],
    /// The cards that each hand has played, with which what it holds now
    /// makes the hand that it bid with.
    played: [Cards; 2],
    /// What the bids of each hand show of the hand that it bid with, as a
    /// minimum and a maximum.
    bounds: [(HandValuation, HandValuation); 2],
    /// The auction.
    bids: Vec<Bid>,
    /// Where in the auction each hand made its calls.
    calls: [Vec<usize>; 2],
}

impl Hidden {
    /// What the player who is to play knows of the hands that they cannot
    /// see, or `None` when it is not a time to play.  `convention` is how the
    /// auction is read.
    pub(crate) fn new<C>(game: &GameState<C>, convention: &Convention) -> Option<Hidden> {
        let viewer = game.turn()?;
        let dummy = game.dummy()?;
        let seats: Vec<Seat> = (0..4)
            .map(Seat::from)
            .filter(|&seat| seat != viewer && seat != dummy)
            .collect();
        let seats = [seats[0], seats[1]];
        // The bids of a seat are every fourth bid, and `shown` wants to know
        // how many seats before the next bidder they were made.
        let bid_offset = |seat: Seat| {
            let first = (seat as usize + 4 - game.dealer as usize) % 4;
            (game.bids.len() + 4 - first) % 4
        };
        let hands = |seats: [Seat; 2]| seats.map(|seat| game.hands[seat]);
        let [first, second] = hands(seats);
        Some(Hidden {
            seats,
            sizes: [first.len(), second.len()],
            pool: first + second,
            forbidden: seats.map(|seat| game.shown_out[seat]),
            played: seats.map(|seat| game.original_hands[seat] - game.hands[seat]),
            bounds: seats.map(|seat| convention.shown_bounds(&game.bids, bid_offset(seat))),
            bids: game.bids.clone(),
            calls: seats.map(|seat| {
                (0..game.bids.len())
                    .filter(|&i| game.dealer + i == seat)
                    .collect()
            }),
        })
    }

    /// The seats of the two hands.
    #[cfg(feature = "benchmarks")]
    pub(crate) fn seats(&self) -> [Seat; 2] {
        self.seats
    }

    /// How many of the calls that the two hands made the robot would not have
    /// made with the hands of `deal`, bidding by `convention`, found by asking
    /// the convention for the bid of every hand, which takes most of a
    /// millisecond a deal.  [`Replay::calls_missed`] says the same much faster,
    /// and this is what it is tested against.
    #[cfg(test)]
    pub(crate) fn calls_missed(&self, deal: &[Cards; 2], convention: &Convention) -> usize {
        (0..2)
            .map(|i| {
                let hand = (deal[i] + self.played[i]).values();
                self.calls[i]
                    .iter()
                    .filter(|&&call| {
                        convention.bid_for(&self.bids[..call], &hand) != self.bids[call]
                    })
                    .count()
            })
            .sum()
    }

    /// The auction of the two hands compiled for `convention`, so that a deal
    /// can be tried against it in microseconds.
    pub(crate) fn replay(&self, convention: &Convention) -> Replay<2> {
        Replay::new(convention, &self.bids, &self.calls, self.played)
    }

    /// The `count` of `deals` that come closest to being bid as the auction
    /// was, by `convention`: the deals that the robot would have bid the
    /// auction with, and then those that miss fewest of its calls.  People do
    /// not always bid as the robot does, so there may be no deal that makes
    /// every call, and a deal that misses one may be as likely as one that
    /// makes them all.
    pub(crate) fn closest_to_the_auction(
        &self,
        deals: Vec<[Cards; 2]>,
        convention: &Convention,
        count: usize,
    ) -> Vec<[Cards; 2]> {
        let replay = self.replay(convention);
        let mut ranked: Vec<(usize, [Cards; 2])> = deals
            .into_iter()
            .map(|deal| (replay.calls_missed(&deal), deal))
            .collect();
        ranked.sort_by_key(|&(missed, _)| missed);
        ranked
            .into_iter()
            .take(count)
            .map(|(_, deal)| deal)
            .collect()
    }

    /// An ensemble of `count` deals, or fewer if the cards cannot be dealt.
    /// With `replay`, a convention, it is the deals of `count` times
    /// [`OVERSAMPLE`] that come closest to being bid as the auction was by
    /// that convention.
    pub(crate) fn ensemble(
        &self,
        rng: &mut impl Rng,
        count: usize,
        replay: Option<&Convention>,
    ) -> Vec<[Cards; 2]> {
        let sampled = if replay.is_some() {
            count * OVERSAMPLE
        } else {
            count
        };
        let mut deals: Vec<[Cards; 2]> = (0..sampled)
            .map_while(|_| self.sample(rng).map(|(deal, _)| deal))
            .collect();
        if let Some(convention) = replay {
            deals = self.closest_to_the_auction(deals, convention, count);
        }
        deals
    }

    /// An ensemble of `count` deals, or fewer if the budget of deals to try
    /// runs out first, each of them drawn at random from the deals that
    /// agree with what is known, where a deal that misses `n` of the calls
    /// that `replay` makes is `miss` to the power `n` as likely as one that
    /// misses none.  This is what [`Hidden::ensemble`] with a replay only
    /// approximates, with a chance for deals that miss some calls and none
    /// of the dependence of the ensemble on how many deals were tried.  When
    /// no deal that is tried misses few enough calls it is the closest.
    pub(crate) fn posterior(
        &self,
        rng: &mut impl Rng,
        count: usize,
        replay: &Replay<2>,
        miss: f64,
    ) -> Dealt {
        let mut dealt = Dealt::default();
        let Some(proposer) = self.proposer() else {
            return dealt;
        };
        // The deals with the fewest misses so far, which are all there is
        // when no deal is accepted.
        let mut closest: Vec<[Cards; 2]> = Vec::new();
        let mut fewest = usize::MAX;
        while dealt.tried < BUDGET && dealt.deals.len() < count {
            dealt.tried += 1;
            let deal = proposer.deal(rng);
            let most = if dealt.deals.is_empty() {
                usize::MAX
            } else {
                MOST_MISSED
            };
            let Some(missed) = replay.calls_missed_within(&deal, most) else {
                continue;
            };
            if missed == 0 {
                dealt.exact += 1;
            }
            if missed < fewest {
                fewest = missed;
                closest.clear();
            }
            if missed == fewest && closest.len() < count {
                closest.push(deal);
            }
            if missed <= MOST_MISSED && (missed == 0 || rng.gen_bool(miss.powi(missed as i32))) {
                dealt.deals.push(deal);
            }
        }
        if dealt.deals.is_empty() {
            dealt.deals = closest;
        }
        dealt
    }

    /// Whether `hands` are hands that the auction could have been made with.
    fn fit_the_bidding(&self, hands: &[Cards; 2]) -> bool {
        (0..2).all(|i| {
            let dealt = (hands[i] + self.played[i]).values();
            let (min, max) = self.bounds[i];
            !min.exceeds(dealt) && !dealt.exceeds(max)
        })
    }

    /// How the cards that are not in sight can be dealt to the two hands, so
    /// that the hands are as large as they are and none has a card of a suit
    /// that it has shown out of.  `None` if there is no way to do it.
    fn proposer(&self) -> Option<Proposer> {
        let mut forced = [Cards::EMPTY; 2];
        let mut free = Cards::EMPTY;
        for card in self.pool {
            let card = Cards::singleton(card);
            match (
                !(self.forbidden[0] & card).is_empty(),
                !(self.forbidden[1] & card).is_empty(),
            ) {
                (true, true) => return None,
                (true, false) => forced[1] += card,
                (false, true) => forced[0] += card,
                (false, false) => free += card,
            }
        }
        if forced[0].len() > self.sizes[0] || forced[1].len() > self.sizes[1] {
            return None;
        }
        let first_wants = self.sizes[0] - forced[0].len();
        if first_wants + (self.sizes[1] - forced[1].len()) != free.len() {
            return None;
        }
        Some(Proposer {
            forced,
            free,
            first_wants,
        })
    }

    /// Deal the cards that are not in sight to the two hands, so that the hands
    /// are as large as they are and none has a card of a suit that it has shown
    /// out of.  `None` if there is no way to do it.
    fn deal(&self, rng: &mut impl Rng) -> Option<[Cards; 2]> {
        Some(self.proposer()?.deal(rng))
    }

    /// One deal of the hands, in the order of `seats`, and whether it fits the
    /// auction.  When there is no deal that fits, or too few to be found, it
    /// is one that has the right cards in each hand and nothing else.  `None`
    /// only when the cards cannot be dealt at all, which says that what is
    /// known does not agree with itself.
    pub(crate) fn sample(&self, rng: &mut impl Rng) -> Option<([Cards; 2], bool)> {
        let mut last = None;
        for _ in 0..ATTEMPTS {
            let hands = self.deal(rng)?;
            if self.fit_the_bidding(&hands) {
                return Some((hands, true));
            }
            last = Some(hands);
        }
        last.map(|hands| (hands, false))
    }
}

/// A way to deal the cards that are not in sight, made by
/// [`Hidden::proposer`]: every deal that it makes has the hands the sizes that
/// they have and none has a card of a suit that it has shown out of, and every
/// such deal is as likely as any other.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Proposer {
    /// The cards that each hand must hold, since the other has shown out of
    /// their suits.
    forced: [Cards; 2],
    /// The cards that either may hold.
    free: Cards,
    /// How many of those the first hand holds.
    first_wants: usize,
}

impl Proposer {
    /// One deal, at random.
    pub(crate) fn deal(&self, rng: &mut impl Rng) -> [Cards; 2] {
        let mut rest = self.free;
        let first = rest.pick_rng(rng, self.first_wants).unwrap();
        [self.forced[0] + first, self.forced[1] + rest]
    }
}

/// The calls of hidden hands, compiled so that it is cheap to find out whether
/// the robot would have made them with the hands of a deal.  See
/// [`Hidden::replay`].
#[derive(Debug)]
pub(crate) struct Replay<const N: usize> {
    /// For each hand, each call that it made, with what the table makes of
    /// the auction before it.
    calls: [Vec<(CallMatcher, Bid)>; N],
    /// The cards that each hand has played, which are part of the hand that
    /// it bid with.
    played: [Cards; N],
}

impl<const N: usize> Replay<N> {
    /// The calls of `N` hands in the auction `bids`, where `calls` says for
    /// each hand which of the bids it made, and `played` is the cards that
    /// each has played since, which with the cards of a deal make the hand
    /// that it bid with.  This takes about a millisecond, which a deal of the
    /// hands then does not.
    pub(crate) fn new(
        convention: &Convention,
        bids: &[Bid],
        calls: &[Vec<usize>; N],
        played: [Cards; N],
    ) -> Self {
        Replay {
            calls: std::array::from_fn(|i| {
                calls[i]
                    .iter()
                    .map(|&call| (convention.matcher(&bids[..call]), bids[call]))
                    .collect()
            }),
            played,
        }
    }

    /// How many of the calls would not have been made with the hands of
    /// `deal`.
    pub(crate) fn calls_missed(&self, deal: &[Cards; N]) -> usize {
        self.calls_missed_within(deal, usize::MAX).unwrap()
    }

    /// How many of the calls would not have been made with the hands of
    /// `deal`, or `None` as soon as that is more than `most`.
    pub(crate) fn calls_missed_within(&self, deal: &[Cards; N], most: usize) -> Option<usize> {
        let mut missed = 0;
        for i in 0..N {
            let hand = (deal[i] + self.played[i]).values();
            for (matcher, call) in &self.calls[i] {
                if matcher.bid_for(&hand) != *call {
                    missed += 1;
                    if missed > most {
                        return None;
                    }
                }
            }
        }
        Some(missed)
    }
}

/// How likely it is that a player makes a call other than the one that the
/// robot would, with the hand that they have: the chance that a deal that
/// misses a call is as likely as one that makes it.  People do not bid as the
/// robot does, so a deal that misses a call or two is not out of the question.
pub(crate) const MISS: f64 = 0.1;

/// The most calls that a deal can miss and still be in an ensemble, which is
/// the truncation of the chance `MISS.powi(missed)` that is already below a
/// thousandth.
const MOST_MISSED: usize = 3;

/// How many deals are tried for an ensemble before it is made of what has
/// been found, with the ensemble kept to the deals that fit the auction as
/// well as can be.
const BUDGET: usize = 20_000;

/// An ensemble of deals of the hidden hands, with what it took to make.
#[derive(Debug, Default)]
pub(crate) struct Dealt {
    /// The deals, each the two hands in the order of the seats.
    pub(crate) deals: Vec<[Cards; 2]>,
    /// How many deals were tried to find them.
    pub(crate) tried: usize,
    /// How many of those make every call.
    pub(crate) exact: usize,
}

/// The deal as the solver has it, for a deal of the hands that cannot be seen:
/// the hands in the order of the play, with each card already played to this
/// trick back in its hand, and those cards.
pub(crate) fn starting_with<C>(
    game: &GameState<C>,
    hidden: &Hidden,
    deal: [Cards; 2],
) -> Option<(Starting, Vec<Card>)> {
    let lead = game.lead?;
    let in_trick: Vec<Card> = if game.played.len() == 4 {
        Vec::new()
    } else {
        game.played.clone()
    };
    let mut hands = [Cards::EMPTY; 4];
    for seat in 0..4 {
        let seat = Seat::from(seat);
        let cards = match hidden.seats.iter().position(|&s| s == seat) {
            Some(i) => deal[i],
            None => game.hands[seat],
        };
        hands[(seat as usize + 4 - lead as usize) % 4] = cards;
    }
    for (i, &card) in in_trick.iter().enumerate() {
        hands[i] += Cards::singleton(card);
    }
    Some((
        Starting {
            hands,
            unknown: Cards::EMPTY,
        },
        in_trick,
    ))
}

/// The trump suit of the contract, if there is one.
pub(crate) fn trump_of<C>(game: &GameState<C>) -> Option<Suit> {
    match game.highest_contract_bid() {
        Some(crate::Bid::Suit(_, trump)) => Some(trump),
        _ => None,
    }
}

/// How many of the tricks left the side of whoever leads this trick wins,
/// when everything is known, after `plays` have been made to the trick.
fn trick_value(
    solver: &mut Naive,
    starting: Starting,
    trump: Option<Suit>,
    plays: &mut Vec<Card>,
) -> f64 {
    if plays.len() == 4 {
        let trick = starting.after([plays[0], plays[1], plays[2], plays[3]], trump);
        return (solver.score(trick.starting()) + trick).mean();
    }
    let seat = plays.len();
    let legal = if seat == 0 {
        starting.hands[0]
    } else {
        starting.hands[seat].following_suit(plays[0].suit())
    };
    let mut best: Option<f64> = None;
    for card in legal {
        plays.push(card);
        let value = trick_value(solver, starting, trump, plays);
        plays.pop();
        best = Some(match best {
            // The leader and the partner of the leader want the most tricks.
            Some(best) if seat & 1 == 0 => best.max(value),
            Some(best) => best.min(value),
            None => value,
        });
    }
    best.expect("a hand has to have a card to play")
}

/// For each card in `legal`, the tricks that the side of whoever leads this
/// trick wins by playing it, when `starting` has every hand, with best play
/// after that, where `in_trick` have been played to the trick already.
pub(crate) fn card_values(
    starting: Starting,
    trump: Option<Suit>,
    in_trick: &[Card],
    legal: Cards,
) -> Vec<(Card, f64)> {
    let mut solver = Naive::new(trump);
    let mut plays = in_trick.to_vec();
    legal
        .map(|card| {
            plays.push(card);
            let value = trick_value(&mut solver, starting, trump, &mut plays);
            plays.pop();
            (card, value)
        })
        .collect()
}

/// Whether the player to play a card wants the most tricks for whoever led the
/// trick, which they do when they are on the same side.
pub(crate) fn plays_for_the_leaders<C>(game: &GameState<C>) -> bool {
    let in_trick = if game.played.len() == 4 {
        0
    } else {
        game.played.len()
    };
    in_trick & 1 == 0
}

/// The most tricks left for which a card is chosen by the ensemble.
const MOST_TRICKS: usize = 7;

/// How many deals of the hands that cannot be seen to try each card in, with
/// this many tricks left.
fn deals_for(tricks_left: usize) -> usize {
    match tricks_left {
        0..=3 => 60,
        4 => 40,
        5 => 30,
        6 => 20,
        _ => 12,
    }
}

/// A player who chooses the card with the best mean double-dummy value over an
/// ensemble of the hands that cannot be seen, which fit the auction and the
/// play so far.  With more tricks left than [`MOST_TRICKS`] it is too slow to
/// work the values out, and it plays as [`RandomPlay`] does.
///
/// The hands of the ensemble fit the bounds that the auction shows.  One that
/// [replays the auction](EnsemblePlay::replaying_the_auction) also keeps only
/// the deals that the robot would have bid the auction with, or that come
/// closest to it.
#[derive(Debug)]
pub struct EnsemblePlay {
    rng: StdRng,
    /// The conventions that auctions have been read with, each with the system
    /// it is the convention of, so that each is built only once.
    conventions: Vec<(BiddingSystem, Convention)>,
    /// Whether to keep the deals that come closest to being bid as the
    /// auction was.
    replay: bool,
}

impl EnsemblePlay {
    pub fn new() -> Self {
        Self::with_rng(StdRng::from_entropy())
    }

    /// A player whose choices are the same every time.
    pub fn seeded(seed: u64) -> Self {
        Self::with_rng(StdRng::seed_from_u64(seed))
    }

    fn with_rng(rng: StdRng) -> Self {
        EnsemblePlay {
            rng,
            conventions: Vec::new(),
            replay: false,
        }
    }

    /// This player, choosing by an ensemble of the deals that the robot would
    /// have bid the auction with, or most likely did not quite (see
    /// [`Hidden::posterior`]), or that miss as few of its calls as any.
    pub fn replaying_the_auction(self) -> Self {
        EnsemblePlay {
            replay: true,
            ..self
        }
    }

    /// The ensemble of `count` deals of the hands that the player cannot see,
    /// with what the player knows of them.
    fn ensemble<C>(
        &mut self,
        game: &GameState<C>,
        count: usize,
    ) -> Option<(Hidden, Vec<[Cards; 2]>)> {
        let known = self.conventions.iter().position(|(s, _)| *s == game.system);
        let i = known.unwrap_or_else(|| {
            let convention = game.system.convention();
            self.conventions.push((game.system, convention));
            self.conventions.len() - 1
        });
        let convention = &self.conventions[i].1;
        let hidden = Hidden::new(game, convention)?;
        let deals = if self.replay {
            let replay = hidden.replay(convention);
            hidden.posterior(&mut self.rng, count, &replay, MISS).deals
        } else {
            hidden.ensemble(&mut self.rng, count, None)
        };
        Some((hidden, deals))
    }
}

impl Default for EnsemblePlay {
    fn default() -> Self {
        Self::new()
    }
}

impl PlayAI for EnsemblePlay {
    fn play<C>(&mut self, game: &GameState<C>) -> Card {
        let legal = game.could_be_played();
        let tricks_left = game.hands.iter().map(|hand| hand.len()).max().unwrap_or(0);
        if legal.len() == 1 {
            return legal.min().unwrap();
        }
        if tricks_left > MOST_TRICKS {
            return RandomPlay.play(game);
        }
        let Some((hidden, deals)) = self.ensemble(game, deals_for(tricks_left)) else {
            return RandomPlay.play(game);
        };
        let trump = trump_of(game);
        let mut totals: Vec<(Card, f64)> = legal.map(|card| (card, 0.0)).collect();
        let mut dealt = 0;
        for deal in deals {
            let Some((starting, in_trick)) = starting_with(game, &hidden, deal) else {
                break;
            };
            for (total, (card, value)) in totals
                .iter_mut()
                .zip(card_values(starting, trump, &in_trick, legal))
            {
                debug_assert_eq!(total.0, card);
                total.1 += value;
            }
            dealt += 1;
        }
        if dealt == 0 {
            return RandomPlay.play(game);
        }
        // The best card, and of cards as good, the cheapest.
        let wants_most = plays_for_the_leaders(game);
        let better = |a: f64, b: f64| if wants_most { a > b } else { a < b };
        let best = totals
            .iter()
            .map(|&(_, total)| total)
            .fold(None, |best: Option<f64>, total| match best {
                Some(best) if !better(total, best) => Some(best),
                _ => Some(total),
            })
            .unwrap();
        let cost = |card: Card| card.rank() + if Some(card.suit()) == trump { 13 } else { 0 };
        totals
            .iter()
            .filter(|&&(_, total)| (total - best).abs() < 1e-9)
            .map(|&(card, _)| card)
            .min_by_key(|&card| cost(card))
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::situations::Generator;
    use crate::{Bid, Seated};
    use std::str::FromStr;

    /// Some situations with `tricks_left` tricks left.
    fn games(tricks_left: usize, count: usize) -> Vec<GameState<()>> {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(11);
        let mut games = Vec::new();
        while games.len() < count {
            let json = generator.situation(&mut rng, games.len(), tricks_left);
            games.extend(json.map(|json| serde_json::from_str(&json).unwrap()));
        }
        games
    }

    #[test]
    fn dealt_hands_have_the_right_cards_and_the_hands_that_were_bid() {
        for tricks_left in [12, 7, 3] {
            for game in games(tricks_left, 6) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                // The hands that are really there fit the auction that they
                // were bid with, which says that what the auction is read as
                // showing agrees with the bidding of the robot.
                let actual = hidden.seats.map(|seat| game.hands[seat]);
                assert!(hidden.fit_the_bidding(&actual), "{:?}", game.bids);
                let mut rng = StdRng::seed_from_u64(5);
                let mut fitted = 0;
                for _ in 0..10 {
                    let (deal, fits) = hidden.sample(&mut rng).unwrap();
                    assert_eq!(hidden.pool, deal[0] + deal[1]);
                    assert!((deal[0] & deal[1]).is_empty());
                    for i in 0..2 {
                        assert_eq!(hidden.sizes[i], deal[i].len());
                        assert!((deal[i] & hidden.forbidden[i]).is_empty());
                    }
                    assert_eq!(fits, hidden.fit_the_bidding(&deal));
                    fitted += fits as usize;
                }
                // The actual hands fit, so there is at least one that does.
                assert!(fitted > 0, "{:?}", game.bids);
            }
        }
    }

    #[test]
    fn the_ensemble_has_many_different_deals() {
        let games = games(7, 8);
        let mut with_choices = 0;
        for game in &games {
            let convention = game.system.convention();
            let hidden = Hidden::new(game, &convention).unwrap();
            let mut rng = StdRng::seed_from_u64(9);
            let deals: Vec<[Cards; 2]> = (0..12)
                .map(|_| hidden.sample(&mut rng).unwrap().0)
                .collect();
            let distinct = deals
                .iter()
                .enumerate()
                .filter(|&(i, deal)| !deals[..i].contains(deal))
                .count();
            with_choices += (distinct > 1) as usize;
        }
        // A deal can be settled by what is known, if rarely.
        assert!(
            with_choices + 1 >= games.len(),
            "{with_choices} of {}",
            games.len()
        );
    }

    #[test]
    fn the_opener_of_one_notrump_is_dealt_the_points_that_it_shows() {
        let game = one_notrump_by_west();
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        assert_eq!(Seat::North, game.turn().unwrap());
        assert_eq!([Seat::South, Seat::West], hidden.seats);
        let mut rng = StdRng::seed_from_u64(3);
        let mut fitted = 0;
        for _ in 0..40 {
            let (deal, fits) = hidden.sample(&mut rng).unwrap();
            if fits {
                fitted += 1;
                let points = deal[1].high_card_points();
                assert!((15..=17).contains(&points), "{points} points");
            }
        }
        assert!(fitted > 10, "only {fitted} of 40 deals fit");
    }

    /// How long it takes to value every legal card in one deal, which is what
    /// a choice of `EnsemblePlay` costs for each deal of its ensemble, for the
    /// stages in `ENSEMBLE_STAGES` (by default 8 to 11 tricks left).
    #[test]
    #[ignore]
    fn report_the_time_to_value_the_cards_of_a_deal() {
        let stages: Vec<usize> = std::env::var("ENSEMBLE_STAGES")
            .ok()
            .map(|s| s.split(',').filter_map(|n| n.parse().ok()).collect())
            .unwrap_or(vec![8, 9, 10, 11]);
        for tricks_left in stages {
            let mut times = Vec::new();
            for game in games(tricks_left, 8) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                let mut rng = StdRng::seed_from_u64(1);
                let (deal, _) = hidden.sample(&mut rng).unwrap();
                let (starting, in_trick) = starting_with(&game, &hidden, deal).unwrap();
                let started = std::time::Instant::now();
                card_values(starting, trump_of(&game), &in_trick, game.could_be_played());
                times.push(started.elapsed().as_secs_f64());
                eprintln!("{tricks_left} tricks left: {:.3} s", times.last().unwrap());
            }
            times.sort_by(f64::total_cmp);
            println!(
                "{tricks_left} tricks left: mean {:.3} s, median {:.3} s, most {:.3} s a deal",
                times.iter().sum::<f64>() / times.len() as f64,
                times[times.len() / 2],
                times[times.len() - 1],
            );
        }
    }

    /// West opens 1NT with 15 points in SAYC, and everyone else passes, with
    /// North on lead.  The hands that North cannot see are South's and West's.
    fn one_notrump_by_west() -> GameState<()> {
        let mut game = GameState::<()>::new();
        game.set_system(BiddingSystem::Sayc);
        let mut hands: Seated<Cards> = [
            Cards::from_str("S T98 H T98 D T98 C T987").unwrap(),
            Cards::from_str("S AK2 H KQ3 D Q42 C J543").unwrap(),
            Cards::from_str("S Q76 H J76 D J76 C Q62").unwrap(),
            Cards::EMPTY,
        ]
        .into();
        hands[Seat::East] =
            Cards::ALL - hands[Seat::South] - hands[Seat::West] - hands[Seat::North];
        game.hands = hands;
        game.original_hands = hands;
        game.dealer = Seat::South;
        game.bids = vec![Bid::Pass, Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
        game.lead = Some(Seat::North);
        game
    }

    #[test]
    fn the_hands_that_were_bid_make_every_call_of_their_auction() {
        for tricks_left in [12, 7, 3] {
            for game in games(tricks_left, 6) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                let actual = hidden.seats.map(|seat| game.hands[seat]);
                assert_eq!(
                    0,
                    hidden.calls_missed(&actual, &convention),
                    "{:?}",
                    game.bids
                );
            }
        }
    }

    #[test]
    fn a_replayed_ensemble_makes_every_call_when_it_can() {
        let game = one_notrump_by_west();
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        assert_eq!(vec![0, 4], hidden.calls[0]);
        assert_eq!(vec![1], hidden.calls[1]);
        let mut rng = StdRng::seed_from_u64(4);
        let deals = hidden.ensemble(&mut rng, 20, Some(&convention));
        assert!(deals.len() > 5, "only {} deals", deals.len());
        for deal in &deals {
            assert_eq!(0, hidden.calls_missed(deal, &convention));
            // South passed as dealer, so it has no opening bid.
            assert!(deal[0].high_card_points() < 13, "{}", deal[0]);
        }
        // Without the replay, the ensemble is as large as was asked for, and
        // has deals with which some call would not have been made.
        let deals = hidden.ensemble(&mut rng, 40, None);
        assert_eq!(40, deals.len());
        assert!(deals
            .iter()
            .any(|deal| hidden.calls_missed(deal, &convention) > 0));
    }

    #[test]
    fn a_replayed_ensemble_keeps_the_closest_deals_when_none_makes_every_call() {
        // West opens 7♣, which the robot would never do with any hand.
        let mut game = one_notrump_by_west();
        game.bids[1] = Bid::Suit(7, Suit::Clubs);
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        let mut rng = StdRng::seed_from_u64(6);
        let sampled = hidden.ensemble(&mut rng, 40, None);
        let mut missed: Vec<usize> = sampled
            .iter()
            .map(|deal| hidden.calls_missed(deal, &convention))
            .collect();
        assert!(missed.iter().all(|&m| m > 0));
        let deals = hidden.closest_to_the_auction(sampled, &convention, 10);
        // The ten that miss fewest, whether or not they tie.
        missed.sort();
        let kept: Vec<usize> = deals
            .iter()
            .map(|deal| hidden.calls_missed(deal, &convention))
            .collect();
        assert_eq!(missed[..10], kept[..]);
    }

    /// How many sampled deals the robot would have bid the auction with, and
    /// how long it takes to find out.
    #[test]
    #[ignore]
    fn report_how_many_deals_make_the_auction() {
        for tricks_left in [12, 7, 3] {
            let games = games(tricks_left, 48);
            let mut sampled = 0;
            let mut fit = 0;
            let mut missed = [0usize; 6];
            let mut exact_per_game = Vec::new();
            let mut time = std::time::Duration::ZERO;
            for game in &games {
                let convention = game.system.convention();
                let hidden = Hidden::new(game, &convention).unwrap();
                let mut rng = StdRng::seed_from_u64(8);
                let mut exact = 0;
                for _ in 0..100 {
                    let (deal, fits) = hidden.sample(&mut rng).unwrap();
                    let started = std::time::Instant::now();
                    let m = hidden.calls_missed(&deal, &convention);
                    time += started.elapsed();
                    sampled += 1;
                    fit += fits as usize;
                    missed[m.min(5)] += 1;
                    exact += (m == 0) as usize;
                }
                exact_per_game.push(exact);
            }
            exact_per_game.sort();
            println!(
                "{tricks_left} tricks left: {fit} of {sampled} fit the bounds; calls missed {missed:?}; \
                 deals of 100 that make the auction, by situation {exact_per_game:?}; \
                 {:?} a replay",
                time / sampled as u32
            );
        }
    }

    /// Hands at random, and the hands of `game`, to try a matcher on.
    fn random_hands(rng: &mut StdRng, count: usize) -> Vec<HandValuation> {
        (0..count)
            .map(|_| {
                let mut deck = Cards::ALL;
                deck.pick_rng(rng, 13).unwrap().values()
            })
            .collect()
    }

    #[test]
    fn a_matcher_bids_as_bid_for_does_after_every_call_of_an_auction() {
        let mut rng = StdRng::seed_from_u64(21);
        let mut tried = 0;
        for game in games(12, 24) {
            let convention = game.system.convention();
            let mut hands = random_hands(&mut rng, 40);
            hands.extend(game.original_hands.iter().map(|hand| hand.values()));
            for call in 0..game.bids.len() {
                let matcher = convention.matcher(&game.bids[..call]);
                for hand in &hands {
                    assert_eq!(
                        convention.bid_for(&game.bids[..call], hand),
                        matcher.bid_for(hand),
                        "{:?} after {:?}",
                        hand,
                        &game.bids[..call]
                    );
                    tried += 1;
                }
            }
        }
        assert!(tried > 5000, "{tried}");
    }

    #[test]
    fn a_replay_misses_the_calls_that_calls_missed_does() {
        for tricks_left in [12, 7, 3] {
            for game in games(tricks_left, 12) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                let replay = hidden.replay(&convention);
                let mut rng = StdRng::seed_from_u64(31);
                for _ in 0..20 {
                    let (deal, _) = hidden.sample(&mut rng).unwrap();
                    assert_eq!(
                        hidden.calls_missed(&deal, &convention),
                        replay.calls_missed(&deal)
                    );
                }
            }
        }
    }

    /// How long it takes to find what a table makes of an auction, and to
    /// try a hand against it, compared with `bid_for`.
    #[test]
    #[ignore]
    fn report_the_cost_of_a_matcher() {
        let mut rng = StdRng::seed_from_u64(2);
        let games = games(12, 24);
        let hands = random_hands(&mut rng, 200);
        let (mut compile, mut fast, mut slow, mut calls) = (std::time::Duration::ZERO, std::time::Duration::ZERO, std::time::Duration::ZERO, 0u32);
        for game in &games {
            let convention = game.system.convention();
            for call in 0..game.bids.len() {
                let started = std::time::Instant::now();
                let matcher = convention.matcher(&game.bids[..call]);
                compile += started.elapsed();
                let started = std::time::Instant::now();
                for hand in &hands {
                    std::hint::black_box(matcher.bid_for(hand));
                }
                fast += started.elapsed();
                let started = std::time::Instant::now();
                for hand in &hands[..20] {
                    std::hint::black_box(convention.bid_for(&game.bids[..call], hand));
                }
                slow += started.elapsed();
                calls += 1;
            }
        }
        println!(
            "per call: compile {:?}, matcher {:?} a hand, bid_for {:?} a hand",
            compile / calls,
            fast / (calls * hands.len() as u32),
            slow / (calls * 20)
        );
    }

    /// The mean of `count` of hand `i` of `deals`, and the standard error of
    /// it.
    fn mean_of(deals: &[[Cards; 2]], i: usize, count: impl Fn(Cards) -> usize) -> (f64, f64) {
        let points: Vec<f64> = deals.iter().map(|deal| count(deal[i]) as f64).collect();
        let mean = points.iter().sum::<f64>() / points.len() as f64;
        let variance =
            points.iter().map(|p| (p - mean).powi(2)).sum::<f64>() / (points.len() - 1) as f64;
        (mean, (variance / points.len() as f64).sqrt())
    }

    #[test]
    fn a_posterior_that_allows_no_misses_is_the_deals_that_make_the_auction() {
        // About one in 200 deals makes this auction, so a few dozen is as many
        // as the budget can find.
        let game = one_notrump_by_west();
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        let replay = hidden.replay(&convention);
        let mut rng = StdRng::seed_from_u64(41);
        let dealt = hidden.posterior(&mut rng, 60, &replay, 0.0);
        assert_eq!(60, dealt.deals.len());
        assert_eq!(60, dealt.exact);
        assert!(dealt
            .deals
            .iter()
            .all(|deal| hidden.calls_missed(deal, &convention) == 0));
        // Rejection of deals that do not make the auction, by another route
        // through the code, has the same distribution.
        let mut kept = Vec::new();
        while kept.len() < 60 {
            let (deal, _) = hidden.sample(&mut rng).unwrap();
            if replay.calls_missed(&deal) == 0 {
                kept.push(deal);
            }
        }
        // West holds every honor that the hands can, so what is left to
        // differ is how long their suits are, with no suit longer than five.
        for suit in Suit::ALL {
            for i in 0..2 {
                let length = |hand: Cards| hand.in_suit(suit).len();
                let (a, a_error) = mean_of(&dealt.deals, i, length);
                let (b, b_error) = mean_of(&kept, i, length);
                let error = (a_error.powi(2) + b_error.powi(2)).sqrt();
                assert!(
                    (a - b).abs() < 4.0 * error + 1e-9,
                    "{suit:?} of hand {i}: {a} and {b}, with error {error}"
                );
            }
        }
    }

    #[test]
    fn a_posterior_gives_deals_that_miss_a_call_their_chance() {
        let game = one_notrump_by_west();
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        let replay = hidden.replay(&convention);
        let mut rng = StdRng::seed_from_u64(42);
        let strict = hidden.posterior(&mut rng, 2000, &replay, 0.0);
        let soft = hidden.posterior(&mut rng, 2000, &replay, 0.3);
        let anything = hidden.posterior(&mut rng, 2000, &replay, 1.0);
        let missing = |deals: &[[Cards; 2]]| {
            deals.iter().filter(|deal| replay.calls_missed(deal) > 0).count()
        };
        assert_eq!(0, missing(&strict.deals));
        assert!(missing(&soft.deals) > 0);
        // Allowing three misses is allowing everything that this auction has
        // calls to miss, so the deals are as they are dealt, and most of
        // them miss a call.
        assert!(missing(&anything.deals) > 1000);
        assert!(missing(&soft.deals) < missing(&anything.deals));
        // The opener of 1NT shows more points than a hand that is dealt.
        let points = |hand: Cards| hand.high_card_points();
        let (strict_points, _) = mean_of(&strict.deals, 1, points);
        let (anything_points, _) = mean_of(&anything.deals, 1, points);
        assert!(strict_points > anything_points + 2.0);
    }

    #[test]
    fn a_posterior_is_the_closest_deals_when_none_makes_the_auction() {
        let mut game = one_notrump_by_west();
        game.bids[1] = Bid::Suit(7, Suit::Clubs);
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        let replay = hidden.replay(&convention);
        let mut rng = StdRng::seed_from_u64(43);
        let dealt = hidden.posterior(&mut rng, 20, &replay, MISS);
        assert!(!dealt.deals.is_empty());
        let fewest = dealt
            .deals
            .iter()
            .map(|deal| replay.calls_missed(deal))
            .min()
            .unwrap();
        assert!(fewest > 0);
        assert!(dealt.deals.iter().all(|deal| replay.calls_missed(deal) == fewest));
    }

    #[test]
    fn a_proposer_deals_what_is_known_of_the_hidden_hands() {
        for tricks_left in [12, 7, 3] {
            for game in games(tricks_left, 6) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                let proposer = hidden.proposer().unwrap();
                let mut rng = StdRng::seed_from_u64(44);
                for _ in 0..20 {
                    let deal = proposer.deal(&mut rng);
                    assert_eq!(hidden.pool, deal[0] + deal[1]);
                    assert!((deal[0] & deal[1]).is_empty());
                    for i in 0..2 {
                        assert_eq!(hidden.sizes[i], deal[i].len());
                        assert!((deal[i] & hidden.forbidden[i]).is_empty());
                    }
                }
            }
        }
    }

    /// What it costs to make an ensemble of `ENSEMBLE_COUNT` deals (60 by
    /// default) that is bid as the auction was, the old way (sample four times
    /// as many and replay each by `bid_for`) and by `posterior`.
    #[test]
    #[ignore]
    fn report_the_cost_of_an_ensemble() {
        use std::time::{Duration, Instant};
        let count: usize = std::env::var("ENSEMBLE_COUNT")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(60);
        for tricks_left in [13, 12, 10, 7, 5, 3] {
            let games = games(tricks_left, 48);
            let (mut old, mut compile, mut new) = (Duration::ZERO, Duration::ZERO, Duration::ZERO);
            let (mut tried, mut exact, mut short) = (0, 0, 0);
            let mut slowest = Duration::ZERO;
            let mut old_short = 0;
            for game in &games {
                let convention = game.system.convention();
                let Some(hidden) = Hidden::new(game, &convention) else {
                    continue;
                };
                let mut rng = StdRng::seed_from_u64(5);
                let started = Instant::now();
                let deals = hidden.ensemble(&mut rng, count, Some(&convention));
                old += started.elapsed();
                old_short += (deals.len() < count / 2) as usize;
                let started = Instant::now();
                let replay = hidden.replay(&convention);
                compile += started.elapsed();
                let started = Instant::now();
                let dealt = hidden.posterior(&mut rng, count, &replay, MISS);
                let took = started.elapsed();
                new += took;
                slowest = slowest.max(took + compile / games.len() as u32);
                tried += dealt.tried;
                exact += dealt.exact;
                short += (dealt.deals.len() < count) as usize;
            }
            let n = games.len() as u32;
            println!(
                "{tricks_left:2} tricks left: old {:?}; compile {:?} + deal {:?} (slowest {:?}); \
                 tried {} a situation, {:.0}% exact; ensembles under {count}: new {short}, old (under half) {old_short}",
                old / n,
                compile / n,
                new / n,
                slowest,
                tried / games.len(),
                100.0 * exact as f64 / tried as f64,
            );
        }
    }

    #[test]
    fn the_player_plays_a_legal_card_whatever_the_stage() {
        let mut player = EnsemblePlay::seeded(1);
        for tricks_left in [13, 8, 7, 4, 2] {
            for game in games(tricks_left, 2) {
                let card = player.play(&game);
                assert!(game.could_be_played().contains(card), "{card:?}");
            }
        }
    }

    #[test]
    fn the_player_takes_the_winners_it_can_see() {
        // North, on lead with the ace of spades and a heart, against hands that
        // hold only two spades each outside of North's: whatever the hidden
        // hands are, the ace wins a trick however it is led, so what matters
        // is that the choice is legal and made quickly.
        let mut player = EnsemblePlay::seeded(2);
        for game in games(2, 5) {
            let started = std::time::Instant::now();
            let card = player.play(&game);
            assert!(game.could_be_played().contains(card));
            assert!(started.elapsed().as_secs() < 5);
        }
    }
}
