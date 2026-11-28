//! The opening lead, which is made before the dummy is seen.
//!
//! The opening leader sees only their own hand and the auction, so the three
//! other hands are all hidden.  [`Unseen`] deals them again and again, as
//! [`super::ensemble::Hidden`] does for the two hands that are hidden later
//! in the play, keeping the deals that fit what the auction showed and coming
//! closest to being bid as the auction was.
//!
//! Solving every lead on each of those deals takes seconds a deal, so
//! [`likely_opening_leads`] guesses the order of the leads from the hand and
//! the auction alone.  It scores each card by a few things about it and its
//! suit (its length and honours, which suits partner and the opponents
//! showed, whether it is trumps), with weights fitted to double-dummy
//! results on deals that fit the auctions that the robot bids, by the ignored
//! test `fit_opening_lead_weights`.
//!
//! No feature reads the calls.  The auction enters through the contract,
//! which the rules of the game give, and through what the bidding system's
//! tables say that the calls showed (`Convention::shown_bounds`), which
//! includes what a natural bid says of the bidder's suit.

use super::ensemble::{calls_missed, closest, OVERSAMPLE};
use super::Convention;
use crate::{Bid, GameState, Seat};
use bridge_deck::{Card, Cards, HandValuation, Suit};
use rand::seq::SliceRandom;
use rand::Rng;

/// How many deals to try for one that fits the auction before taking one that
/// does not.
const ATTEMPTS: usize = 1000;

/// The three hands that the opening leader cannot see.
pub(crate) struct Unseen {
    /// The cards that the three hands hold between them.
    pool: Cards,
    /// What the bids of each hand show of it, as a minimum and a maximum.
    bounds: [(HandValuation, HandValuation); 3],
    /// The auction.
    bids: Vec<Bid>,
    /// Where in the auction each hand made its calls.
    calls: [Vec<usize>; 3],
}

impl Unseen {
    /// What the opening leader knows of the other hands, or `None` when it is
    /// not the opening lead.
    pub(crate) fn new<C>(game: &GameState<C>, convention: &Convention) -> Option<Unseen> {
        let leader = game.turn()?;
        if !game.played.is_empty() || game.lead != Some(leader) {
            return None;
        }
        // The dummy, partner and the declarer, in the order of play.
        let seats = [leader + 1, leader + 2, leader + 3];
        Some(Unseen {
            pool: Cards::ALL - game.hands[leader],
            bounds: Unseen::shown(game, convention, leader),
            bids: game.bids.clone(),
            calls: seats.map(|seat| {
                (0..game.bids.len())
                    .filter(|&i| game.dealer + i == seat)
                    .collect()
            }),
        })
    }

    /// What the auction of `game` showed of the dummy, partner and the
    /// declarer, in the order of play from `leader`, as a minimum and a
    /// maximum, by the tables of `convention`.
    pub(crate) fn shown<C>(
        game: &GameState<C>,
        convention: &Convention,
        leader: Seat,
    ) -> [(HandValuation, HandValuation); 3] {
        let bid_offset = |seat: Seat| {
            let first = (seat as usize + 4 - game.dealer as usize) % 4;
            (game.bids.len() + 4 - first) % 4
        };
        [leader + 1, leader + 2, leader + 3]
            .map(|seat| convention.shown_bounds(&game.bids, bid_offset(seat)))
    }

    /// One deal of the three hands, at random.
    fn deal(&self, rng: &mut impl Rng) -> [Cards; 3] {
        let mut cards: Vec<_> = self.pool.collect();
        cards.shuffle(rng);
        let hand = |i: usize| {
            cards[13 * i..13 * (i + 1)]
                .iter()
                .fold(Cards::EMPTY, |hand, &card| hand.insert(card))
        };
        [hand(0), hand(1), hand(2)]
    }

    /// Whether `deal` fits what the auction showed of each hand.
    fn fits(&self, deal: &[Cards; 3]) -> bool {
        (0..3).all(|i| {
            let values = deal[i].values();
            let (min, max) = self.bounds[i];
            !min.exceeds(values) && !values.exceeds(max)
        })
    }

    /// A deal that fits the auction, or after many tries one that does not.
    fn sample(&self, rng: &mut impl Rng) -> [Cards; 3] {
        let mut deal = self.deal(rng);
        for _ in 1..ATTEMPTS {
            if self.fits(&deal) {
                break;
            }
            deal = self.deal(rng);
        }
        deal
    }

    /// How many of the calls of the three hands the robot would not have
    /// made with the hands of `deal`, bidding by `convention`.
    fn calls_missed(&self, deal: &[Cards; 3], convention: &Convention) -> usize {
        let hands = deal.map(|hand| hand.values());
        calls_missed(convention, &self.bids, &self.calls, &hands)
    }

    /// An ensemble of `count` deals that fit the auction, each of them the
    /// four hands in the order of play from the leader, whose hand is `hand`.
    /// With `replay`, a convention, it is the `count` of [`OVERSAMPLE`] times
    /// as many that come closest to being bid as the auction was by that
    /// convention (see [`closest`]).
    pub(crate) fn ensemble(
        &self,
        rng: &mut impl Rng,
        hand: Cards,
        count: usize,
        replay: Option<&Convention>,
    ) -> Vec<[Cards; 4]> {
        let sampled = if replay.is_some() {
            count * OVERSAMPLE
        } else {
            count
        };
        let mut deals: Vec<[Cards; 3]> = (0..sampled).map(|_| self.sample(rng)).collect();
        if let Some(convention) = replay {
            deals = closest(deals, count, |deal| self.calls_missed(deal, convention));
        }
        deals
            .into_iter()
            .map(|[dummy, partner, declarer]| [hand, dummy, partner, declarer])
            .collect()
    }
}

/// What the auction showed of one suit of one hand: the fewest and most cards
/// in it, and the fewest and most high card points in it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SuitShown {
    pub(crate) length: (u8, u8),
    pub(crate) points: (u8, u8),
}

/// What the auction showed of one of the other three hands, as the bids of
/// that hand's table entries state it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct HandShown {
    pub(crate) suits: [SuitShown; 4],
    /// The fewest and most high card points.
    pub(crate) points: (u8, u8),
}

impl HandShown {
    /// What the bounds `min` and `max` on a hand show.
    pub(crate) fn from_bounds(min: &HandValuation, max: &HandValuation) -> HandShown {
        HandShown {
            suits: Suit::ALL.map(|suit| SuitShown {
                length: (min.length[suit], max.length[suit]),
                points: (min.hcp_in_suit[suit], max.hcp_in_suit[suit]),
            }),
            points: (min.hcp, max.hcp),
        }
    }
}

/// What the opening leader knows from the auction that a lead depends on:
/// the contract (its strain, level and whether it is doubled, which the rules
/// of the game give), and what the auction showed of the other three hands,
/// as the bounds that the bidding system's tables state for the calls
/// (`Convention::shown_bounds`).  It does not hold the calls themselves, so
/// that what a lead depends on is only what the system says they showed.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Auction {
    trump: Option<Suit>,
    level: usize,
    doubled: bool,
    /// What was shown of the dummy, partner and the declarer, in the order
    /// of play from the leader.
    shown: [HandShown; 3],
}

/// The seats that `Auction::shown` holds, counting from the leader.
const DUMMY: usize = 0;
const PARTNER: usize = 1;
const DECLARER: usize = 2;

impl Auction {
    /// The auction of `game`, as the player whose turn it is sees it, read by
    /// the tables of the game's bidding system.
    pub(crate) fn new<C>(game: &GameState<C>) -> Option<Auction> {
        let leader = game.turn()?;
        let convention = game.system.table();
        let bounds = Unseen::shown(game, convention, leader);
        let (trump, level, doubled) = contract(&game.bids);
        Some(Auction {
            trump,
            level,
            doubled,
            shown: bounds.map(|(min, max)| HandShown::from_bounds(&min, &max)),
        })
    }

    /// The auction of a contract, and what was shown of each of the hands.
    #[cfg(test)]
    pub(crate) fn from_shown(
        trump: Option<Suit>,
        level: usize,
        doubled: bool,
        shown: [HandShown; 3],
    ) -> Auction {
        Auction {
            trump,
            level,
            doubled,
            shown,
        }
    }

    /// Whether the calls of `seat` showed at least `least` cards in `suit`.
    fn showed(&self, seat: usize, suit: Suit, least: u8) -> bool {
        self.shown[seat].suits[suit as usize].length.0 >= least
    }

    /// Whether either opponent showed at least `least` cards in `suit`.
    fn opponent_showed(&self, suit: Suit, least: u8) -> bool {
        self.showed(DUMMY, suit, least) || self.showed(DECLARER, suit, least)
    }
}

/// The trumps, level, and whether it is doubled, of the contract that `bids`
/// end in.
fn contract(bids: &[Bid]) -> (Option<Suit>, usize, bool) {
    let mut contract = (None, 0, false);
    for &bid in bids {
        match bid {
            Bid::Suit(level, suit) => contract = (Some(suit), level, false),
            Bid::NT(level) => contract = (None, level, false),
            Bid::Double | Bid::Redouble => contract.2 = true,
            Bid::Pass => {}
        }
    }
    contract
}

/// The things about a lead that [`likely_opening_leads`] weighs.
const FEATURES: [&str; 39] = [
    "length of the suit / 4",
    "singleton",
    "doubleton",
    "points in the suit / 4",
    "ace without the king",
    "king without the ace or queen",
    "ace and king",
    "king and queen",
    "queen and jack",
    "jack and ten",
    "trumps",
    "partner showed four cards in the suit",
    "the dummy showed four cards in the suit",
    "the declarer showed four cards in the suit",
    "partner showed five cards in the suit",
    "none of the three showed four cards in the suit",
    "the top card of the suit",
    "the lowest card of the suit",
    "fourth from the top",
    "the top of two touching cards",
    "an honour",
    "an ace",
    "the top of a doubleton",
    "the top of three or more without an honour",
    "a singleton, holding trumps",
    "an ace against a slam",
    "the contract is doubled",
    "the top card, in a suit partner showed four cards in",
    "doubled, leading trumps",
    "the lower of two touching cards",
    "the middle of three touching cards",
    "trumps, with dummy having shown four cards in a side suit",
    "a long suit against a high contract",
    "a long suit an opponent showed four cards in",
    "an ace in a suit an opponent showed four cards in",
    "the top card, in a suit partner showed five cards in",
    "partner showed at most two cards in the suit",
    "partner showed high cards in the suit",
    "an opponent showed at most two cards in the suit",
];

/// The things about leading `card` from `hand` after `auction`, as
/// [`FEATURES`] names them.
fn features(auction: &Auction, hand: Cards, card: Card) -> [f64; FEATURES.len()] {
    let suit = card.suit();
    let holding = hand.in_suit(suit);
    let length = holding.len();
    let has = |rank: u8| holding.contains(Card::new(suit, rank));
    let (ace, king, queen, jack, ten) = (has(14), has(13), has(12), has(11), has(10));
    let higher = (holding & Cards::higher_than(card)).len();
    let trumps = |t: Suit| hand.in_suit(t).len();
    let is_trump = auction.trump == Some(suit);
    let top = higher == 0;
    let partner = auction.shown[PARTNER].suits[suit as usize];
    let honours = ace || king || queen || jack;
    let opponent_four = auction.opponent_showed(suit, 4);
    let opponent_short = [DUMMY, DECLARER]
        .iter()
        .any(|&seat| auction.shown[seat].suits[suit as usize].length.1 <= 2);
    let f = |b: bool| b as u8 as f64;
    [
        length as f64 / 4.0,
        f(length == 1),
        f(length == 2),
        holding.high_card_points() as f64 / 4.0,
        f(ace && !king),
        f(king && !ace && !queen),
        f(ace && king),
        f(king && queen),
        f(queen && jack),
        f(jack && ten),
        f(is_trump),
        f(auction.showed(PARTNER, suit, 4)),
        f(auction.showed(DUMMY, suit, 4)),
        f(auction.showed(DECLARER, suit, 4)),
        f(auction.showed(PARTNER, suit, 5)),
        f(![DUMMY, PARTNER, DECLARER]
            .iter()
            .any(|&seat| auction.showed(seat, suit, 4))),
        f(top),
        f(higher + 1 == length),
        f(higher == 3),
        f(card.rank() > 2 && holding.contains(Card::new(suit, card.rank() - 1))),
        f(card.rank() >= 10),
        f(card.rank() == 14),
        f(length == 2 && top),
        f(length >= 3 && top && !honours),
        f(length == 1 && !is_trump && auction.trump.is_some_and(|t| trumps(t) > 0)),
        f(card.rank() == 14 && auction.level >= 6),
        f(auction.doubled),
        f(top && auction.showed(PARTNER, suit, 4)),
        f(auction.doubled && is_trump),
        f(card.rank() < 14 && has(card.rank() + 1)),
        f(card.rank() > 2 && card.rank() < 14 && has(card.rank() + 1) && has(card.rank() - 1)),
        f(is_trump
            && Suit::ALL
                .iter()
                .any(|&s| s != suit && auction.showed(DUMMY, s, 4))),
        f(length >= 4 && auction.level >= 5),
        f(length >= 4 && opponent_four),
        f(card.rank() == 14 && opponent_four),
        f(top && auction.showed(PARTNER, suit, 5)),
        f(partner.length.1 <= 2),
        f(partner.points.0 >= 3),
        f(opponent_short),
    ]
}

/// The weights of [`FEATURES`] against a notrump contract, fitted to the
/// 3,626 notrump leads of 12,000 fresh deals (seed 31) that the lead
/// conventions allow, each valued by eight playouts of the big play model in
/// every seat (see `collect_lead_playouts` and `fit_lead_playouts`).
const NOTRUMP_WEIGHTS: [f64; FEATURES.len()] = [
    1.287, -0.046, -0.142, 1.197, -0.775, -0.774, 0.275, 0.908, 1.238, 0.900, 0.000, 1.432, -0.763,
    -0.921, 1.425, 0.251, -1.483, -0.836, 1.336, 1.360, -1.757, -0.721, -0.079, 0.942, 0.000, 0.001,
    0.000, 1.432, 0.000, 0.626, 1.287, 0.000, -0.049, -1.735, -0.063, 1.425, 0.000, 0.000, -0.374,
];

/// The weights of [`FEATURES`] against a suit contract, fitted to 3,499
/// suit leads of the same 5,000 situations.
const SUIT_WEIGHTS: [f64; FEATURES.len()] = [
    -1.024, 2.894, 1.309, -0.933, -2.646, -2.885, 2.659, 1.687, 1.010, 0.576, -2.259, 1.494, -0.476,
    -2.686, 1.343, 1.124, 2.946, 1.687, -0.877, 4.357, -0.097, 3.812, 1.459, -0.232, 2.602, -0.018,
    -0.000, 1.262, -0.134, 0.270, 0.223, -0.612, 0.109, -0.791, 0.101, 1.092, 0.005, 0.000, -0.024,
];

/// How likely a lead of `card` is to be best, the higher the better.
fn lead_score(auction: &Auction, hand: Cards, card: Card, weights: &[f64; FEATURES.len()]) -> f64 {
    features(auction, hand, card)
        .iter()
        .zip(weights)
        .map(|(feature, weight)| feature * weight)
        .sum()
}

/// The cards of `hand`, the leads that are likeliest best after `auction`
/// first, as far as can be told from the hand and the auction alone.
pub(crate) fn likely_opening_leads(auction: &Auction, hand: Cards) -> Vec<Card> {
    let weights = if auction.trump.is_some() {
        &SUIT_WEIGHTS
    } else {
        &NOTRUMP_WEIGHTS
    };
    let allowed: Cards = hand.filter(|&card| lead_allowed(auction, hand, card)).fold(Cards::EMPTY, Cards::insert);
    // Every hand has a suit of four or more, so some lead is always allowed;
    // the whole hand is only a guard.
    let candidates = if allowed.is_empty() { hand } else { allowed };
    let mut leads: Vec<(f64, Card)> = candidates
        .map(|card| (lead_score(auction, hand, card, weights), card))
        .collect();
    leads.sort_by(|a, b| b.0.total_cmp(&a.0));
    leads.into_iter().map(|(_, card)| card).collect()
}

/// Whether the opening lead of `card` from `hand` keeps the partnership's
/// lead conventions.  In partner's suit (one that partner's calls showed at
/// least four cards of, by the tables' bounds), in any contract, the lead is
/// the highest card of the suit.  Against notrump, in any other suit, it is
/// an ace, king or queen, or the fourth-highest card of a suit of four or
/// more, so that a jack or lower is always fourth best.  Against a suit
/// contract any other lead is allowed.
pub(crate) fn lead_allowed(auction: &Auction, hand: Cards, card: Card) -> bool {
    let (partner_rule, fourth_rule) = conventions_on();
    let suit = hand.in_suit(card.suit());
    if partner_rule && partners_suit(auction.shown[PARTNER].suits[card.suit() as usize].length.0) {
        return suit.max() == Some(card);
    }
    !fourth_rule || auction.trump.is_some() || card.rank() >= 12 || fourth_highest(suit) == Some(card)
}

/// Which of the two lead conventions are on: the rule for partner's suit, and
/// the rule for the other suits at notrump.  Both are, except in tests of
/// each alone (see [`with_conventions`]).
#[cfg(not(test))]
fn conventions_on() -> (bool, bool) {
    (true, true)
}

#[cfg(test)]
fn conventions_on() -> (bool, bool) {
    CONVENTIONS.with(std::cell::Cell::get)
}

#[cfg(test)]
thread_local! {
    /// Which lead conventions are on for this thread, as [`conventions_on`]
    /// says: the one for partner's suit, and the one for the other suits at
    /// notrump (set by [`with_conventions`]).
    static CONVENTIONS: std::cell::Cell<(bool, bool)> = const { std::cell::Cell::new((true, true)) };
}

/// Runs `f` on this thread with the lead conventions for partner's suit and
/// for the other suits at notrump on or off (in tests only), which
/// [`lead_allowed`] and [`lead_facts`] follow.
#[cfg(test)]
pub(crate) fn with_conventions<R>(partner_rule: bool, fourth_rule: bool, f: impl FnOnce() -> R) -> R {
    let before = CONVENTIONS.replace((partner_rule, fourth_rule));
    let result = f();
    CONVENTIONS.set(before);
    result
}

/// Whether partner's fewest cards of a suit, as the auction showed them, make
/// it partner's suit for the lead conventions.
fn partners_suit(least: u8) -> bool {
    least >= 4
}

/// The fourth-highest of `cards`, if there are four.
fn fourth_highest(cards: Cards) -> Option<Card> {
    let mut cards: Vec<Card> = cards.collect();
    cards.sort_by_key(|card| std::cmp::Reverse(card.rank()));
    cards.get(3).copied()
}

/// What an opening lead shows by the lead conventions (see
/// [`lead_allowed`]), which the defenders and the declarer both know.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LeadShows {
    /// Fourth best: the leader held exactly three cards of the suit above
    /// the one led, and so at least four, and one more for each lower card
    /// of the suit that they play later.
    FourthBest,
    /// The highest of partner's suit: the leader held none above it.
    Highest,
}

/// The opening lead and what it shows.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct LeadFact {
    pub(crate) leader: Seat,
    pub(crate) card: Card,
    pub(crate) shows: LeadShows,
}

#[cfg(test)]
thread_local! {
    /// Whether [`lead_facts`] reads nothing, for tests of a table whose
    /// players do not know the lead conventions: set by [`with_facts`] for
    /// the thread.
    static FACTS_OFF: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Runs `f` on this thread with [`lead_facts`] on or off (in tests only).
#[cfg(test)]
pub(crate) fn with_facts<R>(on: bool, f: impl FnOnce() -> R) -> R {
    let before = FACTS_OFF.replace(!on);
    let result = f();
    FACTS_OFF.set(before);
    result
}

/// What the opening lead of `game` shows by the lead conventions, if it has
/// been made and shows anything (an ace, king or queen against notrump
/// outside partner's suit, and any lead against a suit contract outside it,
/// show nothing).  It reads only the game: the first card of the play, the
/// contract and what partner's calls showed, so that every player and every
/// robot can know it.  `None` for a game that does not keep its tricks once
/// the first one is over.
pub(crate) fn lead_facts<C>(game: &GameState<C>) -> Option<LeadFact> {
    #[cfg(test)]
    if FACTS_OFF.with(std::cell::Cell::get) {
        return None;
    }
    let leader = game.dummy()? + 3;
    let card = match game.tricks.first() {
        Some(&(seat, cards)) if seat == leader => cards[0],
        Some(_) => return None,
        None if game.ns_tricks + game.ew_tricks == 0 => *game.played.first()?,
        None => return None,
    };
    let bounds = Unseen::shown(game, game.system.table(), leader);
    let (trump, _, _) = contract(&game.bids);
    let (partner_rule, fourth_rule) = conventions_on();
    let shows = if partner_rule && partners_suit(bounds[PARTNER].0.length[card.suit()]) {
        LeadShows::Highest
    } else if fourth_rule && trump.is_none() && card.rank() <= 11 {
        LeadShows::FourthBest
    } else {
        return None;
    };
    Some(LeadFact { leader, card, shows })
}

/// Other ways of ordering the opening leads, beside [`likely_opening_leads`],
/// for the fits in the tests and for the "opening lead" benchmarks to compare.
#[cfg(any(test, feature = "benchmarks"))]
pub(crate) mod candidates {
    use super::*;
    use crate::situations::Generator;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    /// Opening leads that the robot bid to, `count` of them, the same every
    /// time for the same `seed`.
    pub(crate) fn opening_leads(seed: u64, count: usize) -> Vec<GameState<()>> {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(seed);
        let mut games = Vec::new();
        let mut index = 10;
        while games.len() < count {
            // The tenth of each sixteen situations is the defender on lead.
            if let Some(json) = generator.situation(&mut rng, index, 13) {
                let game: GameState<()> = serde_json::from_str(&json).unwrap();
                if game.played.is_empty() {
                    games.push(game);
                }
            }
            index += 16;
        }
        games
    }

    #[cfg(feature = "benchmarks")]
    /// The lead from `hand` that `score` scores highest.
    pub(crate) fn best_lead(hand: Cards, score: impl Fn(Card) -> f64) -> Card {
        let mut best = (f64::MIN, None);
        for card in hand {
            let s = score(card);
            if s > best.0 {
                best = (s, Some(card));
            }
        }
        best.1.unwrap()
    }

    #[cfg(feature = "benchmarks")]
    /// The first of [`likely_opening_leads`], without sorting the others.
    pub(crate) fn linear_lead(auction: &Auction, hand: Cards) -> Card {
        let weights = if auction.trump.is_some() {
            &SUIT_WEIGHTS
        } else {
            &NOTRUMP_WEIGHTS
        };
        best_lead(hand, |card| lead_score(auction, hand, card, weights))
    }

    #[cfg(feature = "benchmarks")]
    /// The lead that an n-tuple network with `weights` scores highest.
    pub(crate) fn tuple_lead(weights: &[f64], auction: &Auction, hand: Cards) -> Card {
        best_lead(hand, |card| tuple_score(weights, &tuple_entries(auction, hand, card)))
    }

    #[cfg(feature = "benchmarks")]
    /// The lead that [`pruned_suit_score`] scores highest.
    pub(crate) fn pruned_lead(auction: &Auction, hand: Cards) -> Card {
        best_lead(hand, |card| pruned_suit_score(auction, hand, card))
    }

    /// The lead that the weights `notrump` or `suit` score highest, among the
    /// leads that the conventions allow if `constrained`, or among all the
    /// cards of the hand if not.
    #[cfg(test)]
    pub(crate) fn lead_by(auction: &Auction, hand: Cards, constrained: bool, notrump: &[f64; FEATURES.len()], suit: &[f64; FEATURES.len()]) -> Card {
        let weights = if auction.trump.is_some() { suit } else { notrump };
        let allowed: Cards = hand.filter(|&card| !constrained || lead_allowed(auction, hand, card)).fold(Cards::EMPTY, Cards::insert);
        let candidates = if allowed.is_empty() { hand } else { allowed };
        candidates
            .max_by(|&a, &b| lead_score(auction, hand, a, weights).total_cmp(&lead_score(auction, hand, b, weights)))
            .unwrap()
    }

    /// The weights of the code, for notrump and suit contracts.
    #[cfg(test)]
    pub(crate) fn current_weights() -> ([f64; FEATURES.len()], [f64; FEATURES.len()]) {
        (NOTRUMP_WEIGHTS, SUIT_WEIGHTS)
    }

    /// The weights of [`FEATURES`] against notrump before the lead
    /// conventions, which the unconstrained leads of a match start from.
    #[cfg(test)]
    pub(crate) const OLD_NOTRUMP_WEIGHTS: [f64; FEATURES.len()] = [
        2.766, -0.064, -0.437, 1.541, -2.635, -1.428, 2.199, 1.598, 1.272, -0.317, 0.000, 2.710, -1.624,
        -2.589, 2.499, 1.484, -1.498, -1.330, -0.571, 0.302, 2.631, 3.948, 0.072, 0.018, 0.000, 0.039,
        0.000, 0.181, 0.000, 7.299, -1.306, 0.000, 0.026, -3.275, 0.058, 0.184, 0.000, -0.008, -0.317,
    ];

    /// The tables of an n-tuple network for the opening lead: their sizes,
    /// and the plain facts about a lead that index each of them.
    pub(crate) const TUPLES: [(usize, &str); 6] = [
        (13 * 64, "the rank, and which of the three ranks above and below it are held"),
        (32 * 7, "which of the ace to the ten of the suit are held, and its length"),
        (13 * 7, "the rank and the length of the suit"),
        (64, "who showed four cards in the suit, whether it is trumps, whether the contract is doubled"),
        (8 * 7, "the level and the length of the suit"),
        (7 * 7, "the length of the trumps and of the suit"),
    ];

    /// The entries of the tables of `TUPLES`, laid end to end, that leading
    /// `card` from `hand` after `auction` looks up, one in each table.
    pub(crate) fn tuple_entries(auction: &Auction, hand: Cards, card: Card) -> [usize; TUPLES.len()] {
        let suit = card.suit();
        let rank = card.rank() as u32;
        let (above, below) = (hand.above(card, 3) as u32, hand.below(card, 3) as u32);
        let length = hand.in_suit(suit).len().min(7) as u32 - 1;
        // Who showed four cards in the suit: bit 0 is the leader, who shows nothing.
        let bid = [DUMMY, PARTNER, DECLARER]
            .iter()
            .fold(0, |bits, &seat| bits | (auction.showed(seat, suit, 4) as u32) << (seat + 1));
        let trump_length = auction.trump.map_or(0, |t| hand.in_suit(t).len().min(6)) as u32;
        let indices = [
            (rank - 2) * 64 + above * 8 + below,
            hand.top_ranks(suit, 5) as u32 * 7 + length,
            (rank - 2) * 7 + length,
            bid | ((auction.trump == Some(suit)) as u32) << 4 | (auction.doubled as u32) << 5,
            auction.level as u32 * 7 + length,
            trump_length * 7 + length,
        ];
        let mut entries = [0; TUPLES.len()];
        let mut offset = 0;
        for (t, index) in indices.into_iter().enumerate() {
            debug_assert!((index as usize) < TUPLES[t].0, "{}", TUPLES[t].1);
            entries[t] = offset + index as usize;
            offset += TUPLES[t].0;
        }
        entries
    }

    #[cfg(feature = "benchmarks")]
    /// The score of a lead in an n-tuple network: the sum of its entries.
    pub(crate) fn tuple_score(weights: &[f64], entries: &[usize]) -> f64 {
        entries.iter().map(|&e| weights[e]).sum()
    }

    #[cfg(feature = "benchmarks")]
    /// A pruned network for suit contracts, fitted to plain facts about the
    /// lead (the length, rank and honours of its suit, who bid it and so on),
    /// written out as its closed-form sum.
    pub(crate) fn pruned_suit_score(auction: &Auction, hand: Cards, card: Card) -> f64 {
        let suit = card.suit();
        let holding = hand.in_suit(suit);
        let has = |rank: u8| holding.contains(Card::new(suit, rank)) as u8 as f64;
        // Who showed four cards in the suit, counting the leader as 0.
        let bid = |seat: usize| {
            (seat > 0 && auction.showed(seat - 1, suit, 4)) as u8 as f64
        };
        let rank = card.rank();
        let length = holding.len() as f64 / 4.0;
        let higher = (holding & Cards::higher_than(card)).len() as f64 / 4.0;
        let (ace, king, queen, jack, ten) = (has(14), has(13), has(12), has(11), has(10));
        let points = holding.high_card_points() as f64 / 4.0;
        let touching_above = if rank < 14 { has(rank + 1) } else { 0.0 };
        let touching_below = if rank > 2 { has(rank - 1) } else { 0.0 };
        let trumps = (auction.trump == Some(suit)) as u8 as f64;
        let level = auction.level as f64 / 7.0;
        let doubled = auction.doubled as u8 as f64;
        let dummy_bid_a_side_suit =
            Suit::ALL.iter().any(|&s| s != suit && auction.showed(DUMMY, s, 4)) as u8 as f64;
        let trump_length = auction.trump.map_or(0.0, |t| hand.in_suit(t).len() as f64 / 4.0);
        let relu = |x: f64| x.max(0.0);
        -0.15 * ace - 0.08 * bid(3) - 0.52 * relu(0.12 * ace) - 0.57 * relu(0.14 * ace)
            + 4.59
                * relu(
                    2.69 - 1.74 * length + 0.76 * higher - 0.97 * king + 0.17 * queen - 0.31 * ten
                        - 0.98 * points
                        + 0.25 * touching_above
                        - 0.72 * touching_below
                        - 0.13 * bid(0)
                        - 0.44 * bid(1)
                        + 0.76 * bid(2)
                        - 1.13 * bid(3)
                        + 0.42 * level
                        - 0.16 * dummy_bid_a_side_suit
                        + 0.91 * trump_length,
                )
            + 4.78
                * relu(
                    -0.93 - 0.49 * length + 1.34 * (rank as f64 / 14.0) - 3.47 * higher
                        + 0.06 * ace
                        + 0.36 * king
                        - 0.15 * queen
                        + 0.04 * jack
                        + 0.25 * ten
                        + 0.76 * points
                        + 1.76 * touching_below
                        - 0.38 * trumps
                        + 0.16 * bid(0)
                        - 0.13 * bid(1)
                        - 0.59 * bid(3)
                        + 0.04 * level
                        - 0.40 * doubled,
                )
    }
}

#[cfg(test)]
mod tests {
    use super::candidates::*;
    use super::*;
    use crate::ai::fitting;
    use crate::ai::ensemble::trump_of;
    use bridge_deck::{Card, Suit};
    use bridge_solver::DoubleDummy;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    /// An auction that showed nothing, but partner's fewest hearts.
    fn auction_with(trump: Option<Suit>, partners_hearts: u8) -> Auction {
        let open = HandShown { suits: [SuitShown { length: (0, 13), points: (0, 10) }; 4], points: (0, 37) };
        let mut partner = open;
        partner.suits[Suit::Hearts as usize].length.0 = partners_hearts;
        Auction::from_shown(trump, 3, false, [open, partner, open])
    }

    fn allowed(auction: &Auction, hand: Cards) -> Cards {
        hand.filter(|&card| lead_allowed(auction, hand, card)).fold(Cards::EMPTY, Cards::insert)
    }

    #[test]
    fn the_lead_conventions_allow_what_they_say() {
        let hand: Cards = "♠KJ852♥852♦AQ3♣74".parse().unwrap();
        let card = |text: &str| -> Cards { text.parse().unwrap() };
        // At notrump: honours, and fourth best from four or more.
        assert_eq!(allowed(&auction_with(None, 0), hand), card("♠K5♦AQ"));
        // In partner's suit, the highest, in any contract.
        assert_eq!(allowed(&auction_with(None, 4), hand), card("♠K5♥8♦AQ"));
        let in_suit = allowed(&auction_with(Some(Suit::Spades), 4), hand);
        assert_eq!(in_suit, hand - card("♥52"));
        // The robot leads only what the conventions allow.
        let auction = auction_with(None, 0);
        assert!(likely_opening_leads(&auction, hand).iter().all(|&c| lead_allowed(&auction, hand, c)));
    }

    /// The robot's leads keep the conventions, and what lead_facts says that
    /// they show is true of the leader's hand.
    #[test]
    fn a_lead_shows_what_lead_facts_says() {
        let (mut fourth, mut highest) = (0, 0);
        for mut game in opening_leads(5, 80) {
            let leader = game.turn().unwrap();
            let hand = game.hands[leader];
            let auction = Auction::new(&game).unwrap();
            let lead = likely_opening_leads(&auction, hand)[0];
            assert!(lead_allowed(&auction, hand, lead));
            assert_eq!(lead_facts(&game), None, "before the lead");
            game.play(lead);
            let suit = hand.in_suit(lead.suit());
            let above = suit.filter(|c| c.rank() > lead.rank()).count();
            match lead_facts(&game) {
                Some(LeadFact { shows: LeadShows::FourthBest, leader: l, card }) => {
                    assert_eq!((l, card), (leader, lead));
                    assert!(auction.trump.is_none() && above == 3 && suit.len() >= 4, "{lead} from {hand}");
                    fourth += 1;
                }
                Some(LeadFact { shows: LeadShows::Highest, card, .. }) => {
                    assert_eq!((card, above), (lead, 0), "{lead} from {hand}");
                    highest += 1;
                }
                None => assert!(auction.trump.is_some() || lead.rank() >= 12, "{lead} from {hand}"),
            }
        }
        assert!(fourth > 0 && highest > 0, "{fourth} fourth best, {highest} in partner's suit");
    }

    /// The live robot's view of the deal places cards by a robot's lead, and
    /// only where they really are.
    #[test]
    fn the_robot_places_cards_by_the_lead_only_where_they_are() {
        use rand::seq::IteratorRandom;
        let mut rng = StdRng::seed_from_u64(9);
        let mut placed = 0;
        for mut game in opening_leads(5, 80) {
            let hand = game.hands[game.turn().unwrap()];
            game.play(likely_opening_leads(&Auction::new(&game).unwrap(), hand)[0]);
            for _ in 0..rng.gen_range(0..12) {
                if game.hand_done {
                    break;
                }
                game.play(game.could_be_played().choose(&mut rng).unwrap());
            }
            let Some(starting) = game.starting() else { continue };
            let lead = game.lead.unwrap();
            for (place, &cards) in starting.hands.iter().enumerate() {
                let seat = lead + place;
                assert!((cards - game.original_hands[seat]).is_empty(), "{seat:?}");
            }
            placed += usize::from(lead_facts(&game).is_some());
        }
        assert!(placed > 0);
    }

    #[test]
    fn the_ensemble_leads_one_of_the_likeliest_leads() {
        use crate::ai::ensemble::EnsemblePlay;
        use crate::ai::PlayAI;
        for game in opening_leads(3, 1) {
            let hand = game.hands[game.turn().unwrap()];
            let likeliest = likely_opening_leads(&Auction::new(&game).unwrap(), hand);
            let lead = EnsemblePlay::seeded(4).replaying_the_auction().play(&game);
            assert!(likeliest[..2].contains(&lead), "{lead} of {likeliest:?}");
        }
    }

    /// How many of the deals of the three unseen hands fit what the auction
    /// showed, which is how much the ensemble has to throw away.
    #[test]
    #[ignore]
    fn report_the_acceptance_of_shown_bounds() {
        let (mut fraction, mut count, mut hopeless) = (0.0, 0, 0);
        for game in opening_leads(7, 150) {
            let convention = game.system.table();
            let unseen = Unseen::new(&game, convention).unwrap();
            let mut rng = StdRng::seed_from_u64(3);
            let fitting = (0..2000)
                .filter(|_| unseen.fits(&unseen.deal(&mut rng)))
                .count();
            fraction += fitting as f64 / 2000.0;
            hopeless += (fitting == 0) as usize;
            count += 1;
        }
        println!(
            "{count} situations: {:.4} of random deals fit the auction; {hopeless} with none of 2000 fitting",
            fraction / count as f64
        );
    }

    #[test]
    fn the_unseen_hands_are_the_other_three() {
        for game in opening_leads(1, 6) {
            let convention = game.system.convention();
            let unseen = Unseen::new(&game, &convention).unwrap();
            let leader = game.turn().unwrap();
            let mut rng = StdRng::seed_from_u64(2);
            for deal in unseen.ensemble(&mut rng, game.hands[leader], 4, Some(&convention)) {
                assert_eq!(game.hands[leader], deal[0]);
                assert_eq!(Cards::ALL, deal[0] + deal[1] + deal[2] + deal[3]);
                assert!(deal.iter().all(|hand| hand.len() == 13));
            }
        }
    }

    /// How long each way of valuing the opening leads takes on a deal, on
    /// `OPENING_LEAD_SITUATIONS` (12) opening leads with two deals each: the
    /// exact tricks of every lead or of the four likeliest, or the tricks of
    /// the deal and then only whether each lead takes them.
    #[test]
    #[ignore]
    fn report_the_cost_of_asking_less() {
        let count = std::env::var("OPENING_LEAD_SITUATIONS")
            .ok()
            .and_then(|n| n.parse().ok())
            .unwrap_or(12);
        let mut times = [0.0; 4];
        let mut deals = 0;
        for game in opening_leads(5, count) {
            let convention = game.system.convention();
            let unseen = Unseen::new(&game, &convention).unwrap();
            let hand = game.hands[game.turn().unwrap()];
            let likely = likely_opening_leads(&Auction::new(&game).unwrap(), hand);
            let trump = trump_of(&game);
            let mut rng = StdRng::seed_from_u64(6);
            for deal in unseen.ensemble(&mut rng, hand, 2, Some(&convention)) {
                let mut time = |i: usize, f: &mut dyn FnMut()| {
                    let started = std::time::Instant::now();
                    f();
                    times[i] += started.elapsed().as_secs_f64();
                };
                time(0, &mut || {
                    DoubleDummy::new(trump).tricks_after_each(deal, &[]);
                });
                time(1, &mut || {
                    let mut solver = DoubleDummy::new(trump);
                    for &lead in &likely[..4] {
                        solver.tricks_after(deal, &[lead]);
                    }
                });
                for (i, leads) in [(2, &likely[..]), (3, &likely[..4])] {
                    time(i, &mut || {
                        let mut solver = DoubleDummy::new(trump);
                        let best = solver.tricks(deal);
                        for &lead in leads {
                            solver.takes_at_least_after(deal, &[lead], best);
                        }
                    });
                }
                deals += 1;
            }
        }
        let names = [
            "exact, every lead",
            "exact, four leads",
            "yes or no, every lead",
            "yes or no, four leads",
        ];
        for (name, time) in names.iter().zip(times) {
            println!("{name}: {:.3} s a deal", time / deals as f64);
        }
    }

    fn bid_name(bid: Bid) -> String {
        let suit = |suit: Suit| ['C', 'D', 'H', 'S'][suit as usize];
        match bid {
            Bid::Pass => "P".into(),
            Bid::Double => "X".into(),
            Bid::Redouble => "XX".into(),
            Bid::Suit(level, s) => format!("{level}{}", suit(s)),
            Bid::NT(level) => format!("{level}N"),
        }
    }

    /// The contract and what was shown of the three hands, as the data holds
    /// them: for each of the dummy, partner and the declarer, each suit's
    /// fewest and most cards and fewest and most points in it, and the fewest
    /// and most points.
    fn contract_json(auction: &Auction) -> serde_json::Value {
        let shown: Vec<_> = auction
            .shown
            .iter()
            .map(|hand| {
                serde_json::json!({
                    "length": hand.suits.map(|s| [s.length.0, s.length.1]),
                    "points": hand.suits.map(|s| [s.points.0, s.points.1]),
                    "hcp": [hand.points.0, hand.points.1],
                })
            })
            .collect();
        serde_json::json!({
            "trump": auction.trump.map(|suit| suit as usize),
            "level": auction.level,
            "doubled": auction.doubled,
            "shown": shown,
        })
    }

    /// The auction that `contract_json` wrote.
    fn read_auction(json: &serde_json::Value) -> Auction {
        let pair = |v: &serde_json::Value| {
            (v[0].as_u64().unwrap() as u8, v[1].as_u64().unwrap() as u8)
        };
        let shown = std::array::from_fn(|seat| {
            let hand = &json["shown"][seat];
            HandShown {
                suits: std::array::from_fn(|suit| SuitShown {
                    length: pair(&hand["length"][suit]),
                    points: pair(&hand["points"][suit]),
                }),
                points: pair(&hand["hcp"]),
            }
        });
        Auction::from_shown(
            json["trump"].as_u64().map(|suit| Suit::ALL[suit as usize]),
            json["level"].as_u64().unwrap() as usize,
            json["doubled"].as_bool().unwrap(),
            shown,
        )
    }

    fn card_pair(card: Card) -> (usize, u8) {
        (card.suit() as usize, card.rank())
    }

    /// Writes, for opening leads that the robot bid to, the double-dummy
    /// tricks that the defenders take with each lead, on deals of the hidden
    /// hands that fit the auction, one JSON object per line, to the file
    /// `OPENING_LEAD_DATA`.  `OPENING_LEAD_SITUATIONS` (400), `OPENING_LEAD_DEALS`
    /// (6) and `OPENING_LEAD_SEED` (1) say how many and which, and
    /// `OPENING_LEAD_THREADS` (all of them) how many threads to use.  The
    /// `contract` says what a lead depends on of the auction (see
    /// [`Auction`]): the dummy, partner and the declarer, in that order, are
    /// the three hands that it shows.  Suits are 0 for clubs to 3 for spades.
    #[test]
    #[ignore]
    fn collect_opening_lead_data() {
        let var = |name: &str, default: u64| {
            std::env::var(name)
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(default)
        };
        let path = std::env::var("OPENING_LEAD_DATA").expect("OPENING_LEAD_DATA");
        let count = var("OPENING_LEAD_SITUATIONS", 400) as usize;
        let deals = var("OPENING_LEAD_DEALS", 6) as usize;
        let seed = var("OPENING_LEAD_SEED", 1);
        let games = opening_leads(seed, count);
        let threads = var(
            "OPENING_LEAD_THREADS",
            std::thread::available_parallelism().map_or(4, |n| n.get()) as u64,
        ) as usize;
        let next = std::sync::atomic::AtomicUsize::new(0);
        let lines = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    let mut conventions = std::collections::HashMap::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(game) = games.get(i) else {
                            break;
                        };
                        let convention = conventions
                            .entry(format!("{:?}", game.system))
                            .or_insert_with(|| game.system.convention());
                        let unseen = Unseen::new(game, convention).unwrap();
                        let leader = game.turn().unwrap();
                        let hand = game.hands[leader];
                        let mut rng = StdRng::seed_from_u64(i as u64);
                        let trump = trump_of(game);
                        let values: Vec<Vec<(usize, u8, usize)>> = unseen
                            .ensemble(&mut rng, hand, deals, Some(convention))
                            .into_iter()
                            .map(|deal| {
                                DoubleDummy::new(trump)
                                    .tricks_after_each(deal, &[])
                                    .into_iter()
                                    .map(|(card, tricks)| {
                                        let (suit, rank) = card_pair(card);
                                        (suit, rank, tricks)
                                    })
                                    .collect()
                            })
                            .collect();
                        let bids: Vec<(usize, String)> = game
                            .bids
                            .iter()
                            .enumerate()
                            .map(|(j, &bid)| {
                                let seat = game.dealer + j;
                                ((seat as usize + 4 - leader as usize) % 4, bid_name(bid))
                            })
                            .collect();
                        let line = serde_json::json!({
                            "situation": i,
                            "system": format!("{:?}", game.system),
                            // The calls are kept only so that another version of
                            // the fit that reads them can be compared on this data.
                            "bids": bids,
                            "contract": contract_json(&Auction::new(game).unwrap()),
                            "hand": hand.map(card_pair).collect::<Vec<_>>(),
                            "trump": trump.map(|suit| suit as usize),
                            "deals": values,
                        });
                        lines.lock().unwrap().push((i, line.to_string()));
                    }
                });
            }
        });
        let mut lines = lines.into_inner().unwrap();
        lines.sort();
        let text: String = lines.into_iter().map(|(_, line)| line + "\n").collect();
        std::fs::write(path, text).unwrap();
    }

    /// An opening lead from the data that `collect_opening_lead_data` writes:
    /// the auction, the hand, and the mean double-dummy tricks of each lead.
    struct Lead {
        auction: Auction,
        hand: Cards,
        values: Vec<(Card, f64)>,
        /// The mean tricks of each lead over the first half of the deals,
        /// and over the rest.
        halves: [Vec<(Card, f64)>; 2],
        /// The tricks of each lead on each deal of the first half.
        first_deals: Vec<Vec<(Card, f64)>>,
    }

    /// The leads in the files `paths`, comma-separated.
    fn read_leads(paths: &str) -> Vec<Lead> {
        let card = |pair: &serde_json::Value| {
            Card::new(
                Suit::ALL[pair[0].as_u64().unwrap() as usize],
                pair[1].as_u64().unwrap() as u8,
            )
        };
        let text: Vec<String> = paths
            .split(',')
            .map(|path| std::fs::read_to_string(path.trim()).unwrap())
            .collect();
        text.iter()
            .flat_map(|text| text.lines())
            .map(|line| {
                let json: serde_json::Value = serde_json::from_str(line).unwrap();
                let hand = json["hand"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(card)
                    .fold(Cards::EMPTY, Cards::insert);
                let deals = json["deals"].as_array().unwrap();
                let means = |deals: &[serde_json::Value]| -> Vec<(Card, f64)> {
                    hand.map(|lead| {
                        let total: u64 = deals
                            .iter()
                            .map(|deal| {
                                let found =
                                    deal.as_array().unwrap().iter().find(|v| card(v) == lead);
                                found.unwrap()[2].as_u64().unwrap()
                            })
                            .sum();
                        (lead, total as f64 / deals.len() as f64)
                    })
                    .collect()
                };
                let half = deals.len().div_ceil(2);
                Lead {
                    auction: read_auction(&json["contract"]),
                    hand,
                    values: means(deals),
                    halves: [means(&deals[..half]), means(&deals[half..])],
                    first_deals: deals[..half]
                        .iter()
                        .map(|deal| means(std::slice::from_ref(deal)))
                        .collect(),
                }
            })
            .filter(|lead: &Lead| lead.halves.iter().all(|half| !half[0].1.is_nan()))
            .collect()
    }

    type Weights = [f64; FEATURES.len()];

    /// The mean tricks of the lead that `choose` makes, over `leads`.
    fn mean_tricks(leads: &[&Lead], mut choose: impl FnMut(&Lead) -> Card) -> f64 {
        let total: f64 = leads
            .iter()
            .map(|lead| {
                let card = choose(lead);
                lead.values.iter().find(|&&(c, _)| c == card).unwrap().1
            })
            .sum();
        total / leads.len() as f64
    }

    fn greedy(lead: &Lead, weights: &Weights) -> Card {
        lead.hand
            .max_by(|&a, &b| {
                lead_score(&lead.auction, lead.hand, a, weights).total_cmp(&lead_score(
                    &lead.auction,
                    lead.hand,
                    b,
                    weights,
                ))
            })
            .unwrap()
    }

    /// Weights that make leads with the most tricks the likeliest, fitted to
    /// `leads` by gradient ascent on the mean tricks of a lead chosen at
    /// random with odds that grow exponentially with its score, less
    /// `penalty` times the size of the weights.
    fn fit(leads: &[&Lead], penalty: f64) -> Weights {
        let mut weights = [0.0; FEATURES.len()];
        let rate = 1.0;
        // `OPENING_LEAD_FEATURES` fits only the first that many features, to
        // see what the rest add.
        let active: usize = std::env::var("OPENING_LEAD_FEATURES")
            .ok()
            .and_then(|n| n.parse().ok())
            .unwrap_or(FEATURES.len());
        for _ in 0..2000 {
            let mut gradient = weights.map(|w| -penalty * w);
            gradient[active..].fill(0.0);
            for lead in leads {
                let features: Vec<Weights> = lead
                    .values
                    .iter()
                    .map(|&(card, _)| features(&lead.auction, lead.hand, card))
                    .collect();
                let scores: Vec<f64> = features
                    .iter()
                    .map(|f| f.iter().zip(&weights).map(|(f, w)| f * w).sum())
                    .collect();
                let most = scores.iter().copied().fold(f64::MIN, f64::max);
                let odds: Vec<f64> = scores.iter().map(|s| (s - most).exp()).collect();
                let sum: f64 = odds.iter().sum();
                let mean: f64 = odds
                    .iter()
                    .zip(&lead.values)
                    .map(|(p, v)| p * v.1)
                    .sum::<f64>()
                    / sum;
                for ((p, (_, value)), f) in odds.iter().zip(&lead.values).zip(&features) {
                    for k in 0..FEATURES.len() {
                        gradient[k] += p / sum * (value - mean) * f[k] / leads.len() as f64;
                    }
                }
            }
            for k in 0..active {
                weights[k] += rate * gradient[k];
            }
        }
        weights
    }

    /// The choices of `leads` for the fits of `fitting`: each lead's entries
    /// of the tables of `TUPLES`, and its mean tricks.
    fn tuple_choices(leads: &[&Lead]) -> fitting::Choices {
        leads
            .iter()
            .map(|lead| {
                lead.values
                    .iter()
                    .map(|&(card, value)| (tuple_entries(&lead.auction, lead.hand, card).to_vec(), value))
                    .collect()
            })
            .collect()
    }

    fn by_contract(leads: &[Lead], notrump: bool) -> Vec<&Lead> {
        leads
            .iter()
            .filter(|lead| lead.auction.trump.is_none() == notrump)
            .collect()
    }

    /// Fits the weights of the lead features to the data in the file
    /// `OPENING_LEAD_DATA`, as `collect_opening_lead_data` writes it, picking
    /// the penalty on the size of the weights by five-fold cross-validation,
    /// and prints the weights.
    #[test]
    #[ignore]
    fn fit_opening_lead_weights() {
        let leads = read_leads(&std::env::var("OPENING_LEAD_DATA").expect("OPENING_LEAD_DATA"));
        for notrump in [true, false] {
            let leads = by_contract(&leads, notrump);
            let mut best = (f64::MIN, 0.0);
            for penalty in [0.0, 0.001, 0.003, 0.01, 0.03, 0.1] {
                let mut total = 0.0;
                for fold in 0..5 {
                    let train: Vec<&Lead> = leads
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| i % 5 != fold)
                        .map(|(_, l)| *l)
                        .collect();
                    let test: Vec<&Lead> = leads
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| i % 5 == fold)
                        .map(|(_, l)| *l)
                        .collect();
                    let weights = fit(&train, penalty);
                    total += mean_tricks(&test, |lead| greedy(lead, &weights)) * test.len() as f64;
                }
                let tricks = total / leads.len() as f64;
                println!("notrump {notrump}, penalty {penalty}: {tricks:.4} tricks held out");
                if tricks > best.0 {
                    best = (tricks, penalty);
                }
            }
            let weights = fit(&leads, best.1);
            println!("{} leads, penalty {}:", leads.len(), best.1);
            for (name, weight) in FEATURES.iter().zip(weights) {
                println!("    {weight:.3}, // {name}");
            }
        }
    }

    /// The leads of the data in the files `paths`, comma-separated, that
    /// `collect_lead_playouts` (in `fitted_play`) writes: the auction and the
    /// hand of each, and the leader's side's mean tricks of each allowed lead.
    fn read_lead_playouts(paths: &str) -> Vec<Lead> {
        let mut games: std::collections::HashMap<u64, Vec<GameState<()>>> = std::collections::HashMap::new();
        let mut leads = Vec::new();
        for path in paths.split(',') {
            let text = std::fs::read_to_string(path.trim()).unwrap();
            let lines: Vec<serde_json::Value> = text.lines().map(|line| serde_json::from_str(line).unwrap()).collect();
            for line in &lines {
                let seed = line["seed"].as_u64().unwrap();
                let most = lines.iter().filter(|l| l["seed"].as_u64() == Some(seed)).map(|l| l["index"].as_u64().unwrap()).max().unwrap();
                games.entry(seed).or_insert_with(|| opening_leads(seed, most as usize + 1));
                let game = &games[&seed][line["index"].as_u64().unwrap() as usize];
                let leader = game.turn().unwrap();
                let values: Vec<(Card, f64)> = line["values"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| (Card::new(Suit::ALL[v[0].as_u64().unwrap() as usize], v[1].as_u64().unwrap() as u8), v[2].as_f64().unwrap()))
                    .collect();
                leads.push(Lead {
                    auction: Auction::new(game).unwrap(),
                    hand: game.hands[leader],
                    values,
                    halves: [Vec::new(), Vec::new()],
                    first_deals: Vec::new(),
                });
            }
        }
        leads
    }

    /// The first of the lead's cards that `weights` score highest.
    fn greedy_of_values(lead: &Lead, weights: &Weights) -> Card {
        lead.values
            .iter()
            .map(|&(card, _)| card)
            .max_by(|&a, &b| lead_score(&lead.auction, lead.hand, a, weights).total_cmp(&lead_score(&lead.auction, lead.hand, b, weights)))
            .unwrap()
    }

    /// Fits the lead weights to the leads of the playouts in the files
    /// `OPENING_LEAD_PLAYOUTS` (see [`read_lead_playouts`]), choosing the
    /// penalty by five-fold cross-validation, and prints, for each kind of
    /// contract, the leader's side's tricks a lead, held out, of the weights
    /// that are in the code, of the new weights, of a lead at random among
    /// those that are allowed, and of the best lead (which is the best of
    /// noisy values, so it is more than any rule can have), the difference of
    /// the new and the old with its standard error, and the new weights.
    #[test]
    #[ignore]
    fn fit_lead_playouts() {
        let leads = read_lead_playouts(&std::env::var("OPENING_LEAD_PLAYOUTS").expect("OPENING_LEAD_PLAYOUTS"));
        // `OPENING_LEAD_ALL` says that the data is of every card of the hand,
        // for the weights of unconstrained leads, which are compared with
        // the weights before the conventions.  `OPENING_LEAD_WEIGHTS_OUT`
        // names a file for the weights that are fitted, a line each for
        // notrump and suit contracts.
        let all = std::env::var("OPENING_LEAD_ALL").is_ok();
        let mut written = String::new();
        let value = |lead: &Lead, card: Card| lead.values.iter().find(|v| v.0 == card).unwrap().1;
        for notrump in [true, false] {
            let leads = by_contract(&leads, notrump);
            let folds = 5;
            let split = |fold: usize| -> (Vec<&Lead>, Vec<&Lead>) {
                let train = leads.iter().enumerate().filter(|(i, _)| i % folds != fold).map(|(_, l)| *l).collect();
                let test = leads.iter().enumerate().filter(|(i, _)| i % folds == fold).map(|(_, l)| *l).collect();
                (train, test)
            };
            let mut best = (f64::MIN, 0.0);
            for penalty in [0.0, 0.001, 0.003, 0.01, 0.03, 0.1] {
                let mut total = 0.0;
                for fold in 0..folds {
                    let (train, test) = split(fold);
                    let weights = fit(&train, penalty);
                    total += mean_tricks(&test, |lead| greedy_of_values(lead, &weights)) * test.len() as f64;
                }
                let tricks = total / leads.len() as f64;
                println!("notrump {notrump}, penalty {penalty}: {tricks:.4} tricks held out");
                if tricks > best.0 {
                    best = (tricks, penalty);
                }
            }
            // Held out: each lead is chosen by weights fitted without it.
            let (mut old, mut new, mut random, mut most) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
            for fold in 0..folds {
                let (train, test) = split(fold);
                let weights = fit(&train, best.1);
                for lead in test {
                    old.push(value(
                        lead,
                        if all {
                            candidates::lead_by(&lead.auction, lead.hand, false, &candidates::OLD_NOTRUMP_WEIGHTS, &SUIT_WEIGHTS)
                        } else {
                            likely_opening_leads(&lead.auction, lead.hand)[0]
                        },
                    ));
                    new.push(value(lead, greedy_of_values(lead, &weights)));
                    random.push(lead.values.iter().map(|v| v.1).sum::<f64>() / lead.values.len() as f64);
                    most.push(lead.values.iter().map(|v| v.1).fold(f64::MIN, f64::max));
                }
            }
            let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len() as f64;
            let differences: Vec<f64> = new.iter().zip(&old).map(|(n, o)| n - o).collect();
            let d = mean(&differences);
            let error = (differences.iter().map(|x| (x - d).powi(2)).sum::<f64>() / (differences.len() as f64 - 1.0) / differences.len() as f64).sqrt();
            println!(
                "{} {} leads, held out, tricks a lead: old weights {:.3}, new weights {:.3} (penalty {}), random allowed lead {:.3}, best of the values {:.3}; new minus old {d:+.3} ({error:.3})",
                leads.len(),
                if notrump { "notrump" } else { "suit" },
                mean(&old),
                mean(&new),
                best.1,
                mean(&random),
                mean(&most),
            );
            let weights = fit(&leads, best.1);
            for (name, weight) in FEATURES.iter().zip(weights) {
                println!("    {weight:.3}, // {name}");
            }
            written += &format!("{}\n", weights.iter().map(|w| w.to_string()).collect::<Vec<_>>().join(","));
        }
        if let Ok(path) = std::env::var("OPENING_LEAD_WEIGHTS_OUT") {
            std::fs::write(path, written).unwrap();
        }
    }

    /// Fits an n-tuple network over the tables of `TUPLES` to the data in
    /// `OPENING_LEAD_DATA` with `fitting`, picking the penalty by five-fold
    /// cross-validation, and prints the tricks of its first lead, and those
    /// of the linear `likely_opening_leads`, on all the deals of the data in
    /// `OPENING_LEAD_TEST`.  `OPENING_LEAD_STEPS` (500) is the number of steps
    /// of each fit.
    #[test]
    #[ignore]
    fn fit_opening_lead_tuples() {
        let leads = read_leads(&std::env::var("OPENING_LEAD_DATA").expect("OPENING_LEAD_DATA"));
        let test = read_leads(&std::env::var("OPENING_LEAD_TEST").expect("OPENING_LEAD_TEST"));
        let steps = std::env::var("OPENING_LEAD_STEPS")
            .ok()
            .and_then(|n| n.parse().ok())
            .unwrap_or(500);
        let size: usize = TUPLES.iter().map(|t| t.0).sum();
        for notrump in [true, false] {
            let leads = tuple_choices(&by_contract(&leads, notrump));
            let test_leads = by_contract(&test, notrump);
            println!(
                "{} {} test leads, likely_opening_leads: {:.4} tricks",
                test_leads.len(),
                if notrump { "notrump" } else { "suit" },
                mean_tricks(&test_leads, |lead| likely_opening_leads(&lead.auction, lead.hand)[0]),
            );
            let penalties = [0.0, 0.0001, 0.001, 0.01];
            let (penalty, held_out) = fitting::cross_validate(&leads, &|| fitting::Network::new(size, &[], 0), &penalties, steps);
            for (penalty, tricks) in held_out {
                println!("  penalty {penalty}: {tricks:.4} tricks held out");
            }
            let weights = fitting::fit(&leads.iter().collect::<Vec<_>>(), fitting::Network::new(size, &[], 0), penalty, steps);
            let test_choices = tuple_choices(&test_leads);
            println!(
                "  best penalty {penalty}: {:.4} tricks on the test leads",
                weights.mean_value(&test_choices.iter().collect::<Vec<_>>()),
            );
        }
    }

    /// Compares the leads of `likely_opening_leads` with those of the robot's
    /// `opening_lead`, a lead at random, and the best lead on the deals of the
    /// data, on the data in the file `OPENING_LEAD_DATA`.  Also says how often
    /// the best lead is among the first few of `likely_opening_leads`, and how
    /// much is lost by solving only those.
    #[test]
    #[ignore]
    fn report_opening_leads() {
        let leads = read_leads(&std::env::var("OPENING_LEAD_DATA").expect("OPENING_LEAD_DATA"));
        let value =
            |values: &[(Card, f64)], card: Card| values.iter().find(|v| v.0 == card).unwrap().1;
        let best_by = |values: &[(Card, f64)], cards: &[Card]| {
            // Of leads as good, the first.
            cards
                .iter()
                .copied()
                .min_by(|&a, &b| value(values, b).total_cmp(&value(values, a)))
                .unwrap()
        };
        for notrump in [true, false] {
            let leads = by_contract(&leads, notrump);
            // Every lead is scored on the second half of the deals, which
            // nothing chooses by but `best`, which is the most there is.
            let score = |choose: &mut dyn FnMut(&Lead) -> f64| {
                leads.iter().map(|lead| choose(lead)).sum::<f64>() / leads.len() as f64
            };
            let all = |lead: &Lead| lead.hand.collect::<Vec<Card>>();
            println!(
                "{} {} leads, tricks on other deals: best {:.3}, likely {:.3}, robot {:.3}, random {:.3}",
                leads.len(),
                if notrump { "notrump" } else { "suit" },
                score(&mut |lead| value(&lead.halves[1], best_by(&lead.halves[1], &all(lead)))),
                score(&mut |lead| value(&lead.halves[1], likely_opening_leads(&lead.auction, lead.hand)[0])),
                // The robot's lead is partly at random, so take the mean of many.
                score(&mut |lead| {
                    (0..20)
                        .map(|_| value(&lead.halves[1], super::super::opening_lead(lead.auction.trump, lead.hand)))
                        .sum::<f64>()
                        / 20.0
                }),
                score(&mut |lead| all(lead).iter().map(|&c| value(&lead.halves[1], c)).sum::<f64>() / 13.0),
            );
            // An ensemble that solves only the first k leads, choosing by
            // the first half of the deals.
            for k in [1, 2, 3, 4, 6, 13] {
                let tricks = score(&mut |lead| {
                    let first = likely_opening_leads(&lead.auction, lead.hand);
                    value(&lead.halves[1], best_by(&lead.halves[0], &first[..k]))
                });
                println!("  ensemble of the first {k}: {tricks:.3}");
            }
            // An ensemble that asks less of the solver: on each deal, only
            // whether each lead loses nothing to the best lead there, or
            // also whether it loses no more than one trick.
            for cap in [1.0, 2.0] {
                for k in [4, 13] {
                    let tricks = score(&mut |lead| {
                        let first = likely_opening_leads(&lead.auction, lead.hand);
                        let first = &first[..k];
                        let loss = |card: Card| -> f64 {
                            lead.first_deals
                                .iter()
                                .map(|deal| {
                                    let best = deal.iter().map(|v| v.1).fold(f64::MIN, f64::max);
                                    (best - value(deal, card)).min(cap)
                                })
                                .sum()
                        };
                        let chosen = first
                            .iter()
                            .copied()
                            .min_by(|&a, &b| loss(a).total_cmp(&loss(b)))
                            .unwrap();
                        value(&lead.halves[1], chosen)
                    });
                    println!("  ensemble of the first {k}, losses capped at {cap}: {tricks:.3}");
                }
            }
        }
    }
}

