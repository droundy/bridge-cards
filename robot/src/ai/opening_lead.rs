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
//! suit (its length and honours, who bid it, whether it is trumps), with
//! weights fitted to double-dummy results on deals that fit the auctions that
//! the robot bids, by the ignored test `fit_opening_lead_weights`.

use super::ensemble::OVERSAMPLE;
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
        let bid_offset = |seat: Seat| {
            let first = (seat as usize + 4 - game.dealer as usize) % 4;
            (game.bids.len() + 4 - first) % 4
        };
        Some(Unseen {
            pool: Cards::ALL - game.hands[leader],
            bounds: seats.map(|seat| convention.shown_bounds(&game.bids, bid_offset(seat))),
            bids: game.bids.clone(),
            calls: seats.map(|seat| {
                (0..game.bids.len())
                    .filter(|&i| game.dealer + i == seat)
                    .collect()
            }),
        })
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
        (0..3)
            .map(|i| {
                let hand = deal[i].values();
                self.calls[i]
                    .iter()
                    .filter(|&&call| {
                        convention.bid_for(&self.bids[..call], &hand) != self.bids[call]
                    })
                    .count()
            })
            .sum()
    }

    /// An ensemble of `count` deals that fit the auction, each of them the
    /// four hands in the order of play from the leader, whose hand is `hand`.
    /// With `replay`, a convention, it is those of [`OVERSAMPLE`] times as
    /// many that come closest to being bid as the auction was by that
    /// convention, up to `count` of them.
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
            let missed: Vec<usize> = deals
                .iter()
                .map(|deal| self.calls_missed(deal, convention))
                .collect();
            let fewest = missed.iter().copied().min().unwrap_or(0);
            deals = deals
                .into_iter()
                .zip(missed)
                .filter(|&(_, missed)| missed == fewest)
                .map(|(deal, _)| deal)
                .take(count)
                .collect();
        }
        deals
            .into_iter()
            .map(|[dummy, partner, declarer]| [hand, dummy, partner, declarer])
            .collect()
    }
}

/// What the opening leader knows from the auction that a lead depends on.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Auction {
    trump: Option<Suit>,
    level: usize,
    /// Which suits each player bid, counting from the leader: 0 is the
    /// leader, 1 the dummy, 2 partner and 3 the declarer.
    bid: [[bool; 4]; 4],
    /// Who doubled or redoubled the contract last, counting from the leader.
    doubled_by: Option<usize>,
}

impl Auction {
    /// The auction of `game`, as the player whose turn it is sees it.
    pub(crate) fn new<C>(game: &GameState<C>) -> Option<Auction> {
        let leader = game.turn()?;
        Some(Auction::from_bids(game.bids.iter().enumerate().map(
            |(i, &bid)| {
                let seat = game.dealer + i;
                ((seat as usize + 4 - leader as usize) % 4, bid)
            },
        )))
    }

    /// The auction of `bids`, each with its bidder counting from the leader.
    fn from_bids(bids: impl Iterator<Item = (usize, Bid)>) -> Auction {
        let mut auction = Auction {
            trump: None,
            level: 0,
            bid: [[false; 4]; 4],
            doubled_by: None,
        };
        for (seat, bid) in bids {
            match bid {
                Bid::Suit(level, suit) => {
                    auction.bid[seat][suit as usize] = true;
                    auction.trump = Some(suit);
                    auction.level = level;
                    auction.doubled_by = None;
                }
                Bid::NT(level) => {
                    auction.trump = None;
                    auction.level = level;
                    auction.doubled_by = None;
                }
                Bid::Double | Bid::Redouble => auction.doubled_by = Some(seat),
                Bid::Pass => {}
            }
        }
        auction
    }
}

/// The things about a lead that [`likely_opening_leads`] weighs.
const FEATURES: [&str; 35] = [
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
    "partner bid the suit",
    "the dummy bid the suit",
    "the declarer bid the suit",
    "the leader bid the suit",
    "nobody bid the suit",
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
    "partner doubled, leading a suit an opponent bid",
    "doubled, leading trumps",
    "the lower of two touching cards",
    "the middle of three touching cards",
    "trumps, with dummy having bid a side suit",
    "a long suit against a high contract",
    "a long suit an opponent bid",
    "an ace in a suit an opponent bid",
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
    let bid = |seat: usize| auction.bid[seat][suit as usize];
    let honours = ace || king || queen || jack;
    let opponent_bid = bid(1) || bid(3);
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
        f(bid(2)),
        f(bid(1)),
        f(bid(3)),
        f(bid(0)),
        f(!(0..4).any(bid)),
        f(higher == 0),
        f(higher + 1 == length),
        f(higher == 3),
        f(card.rank() > 2 && holding.contains(Card::new(suit, card.rank() - 1))),
        f(card.rank() >= 10),
        f(card.rank() == 14),
        f(length == 2 && higher == 0),
        f(length >= 3 && higher == 0 && !honours),
        f(length == 1 && !is_trump && auction.trump.is_some_and(|t| trumps(t) > 0)),
        f(card.rank() == 14 && auction.level >= 6),
        f(auction.doubled_by.is_some()),
        f(auction.doubled_by == Some(2) && opponent_bid),
        f(auction.doubled_by.is_some() && is_trump),
        f(card.rank() < 14 && has(card.rank() + 1)),
        f(card.rank() > 2 && card.rank() < 14 && has(card.rank() + 1) && has(card.rank() - 1)),
        f(is_trump && (0..4).any(|s| s != suit as usize && auction.bid[1][s])),
        f(length >= 4 && auction.level >= 5),
        f(length >= 4 && opponent_bid),
        f(card.rank() == 14 && opponent_bid),
    ]
}

/// The weights of [`FEATURES`] against a notrump contract.
const NOTRUMP_WEIGHTS: [f64; FEATURES.len()] = [
    2.693, 0.005, -0.082, 0.807, -1.341, -0.972, 1.384, 1.145, -0.444, -0.186,
    0.000, 1.197, -1.150, -1.500, 0.110, 1.437, -1.925, -0.958, -0.526, -0.319,
    1.188, 0.201, -0.121, -0.115, 0.000, 0.002, 0.000, 0.000, 0.000, 4.110,
    -0.369, 0.000, -0.214, -1.098, 0.009,
];

/// The weights of [`FEATURES`] against a suit contract.
const SUIT_WEIGHTS: [f64; FEATURES.len()] = [
    -0.492, 2.708, 1.398, -0.608, -2.871, -3.044, 2.321, 1.879, 1.007, 1.226,
    -2.189, 2.375, -1.021, -2.851, 0.544, 0.271, 2.680, 1.399, -0.976, 3.927,
    -0.352, 4.138, 1.570, -0.510, 2.573, 0.101, -0.000, 0.000, -0.010, 0.595,
    0.475, -0.324, 0.147, -0.198, 0.358,
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
    let mut leads: Vec<(f64, Card)> = hand
        .map(|card| (lead_score(auction, hand, card, weights), card))
        .collect();
    leads.sort_by(|a, b| b.0.total_cmp(&a.0));
    leads.into_iter().map(|(_, card)| card).collect()
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

    /// The tables of an n-tuple network for the opening lead: their sizes,
    /// and the plain facts about a lead that index each of them.
    pub(crate) const TUPLES: [(usize, &str); 6] = [
        (13 * 64, "the rank, and which of the three ranks above and below it are held"),
        (32 * 7, "which of the ace to the ten of the suit are held, and its length"),
        (13 * 7, "the rank and the length of the suit"),
        (64, "who bid the suit, whether it is trumps, whether the contract is doubled"),
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
        let bid = (0..4).fold(0, |bits, seat| bits | (auction.bid[seat][suit as usize] as u32) << seat);
        let trump_length = auction.trump.map_or(0, |t| hand.in_suit(t).len().min(6)) as u32;
        let indices = [
            (rank - 2) * 64 + above * 8 + below,
            hand.top_ranks(suit, 5) as u32 * 7 + length,
            (rank - 2) * 7 + length,
            bid | ((auction.trump == Some(suit)) as u32) << 4 | (auction.doubled_by.is_some() as u32) << 5,
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
        let bid = |seat: usize| auction.bid[seat][suit as usize] as u8 as f64;
        let rank = card.rank();
        let length = holding.len() as f64 / 4.0;
        let higher = (holding & Cards::higher_than(card)).len() as f64 / 4.0;
        let (ace, king, queen, jack, ten) = (has(14), has(13), has(12), has(11), has(10));
        let points = holding.high_card_points() as f64 / 4.0;
        let touching_above = if rank < 14 { has(rank + 1) } else { 0.0 };
        let touching_below = if rank > 2 { has(rank - 1) } else { 0.0 };
        let trumps = (auction.trump == Some(suit)) as u8 as f64;
        let level = auction.level as f64 / 7.0;
        let doubled = auction.doubled_by.is_some() as u8 as f64;
        let dummy_bid_a_side_suit =
            (0..4).any(|s| s != suit as usize && auction.bid[1][s]) as u8 as f64;
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

    fn card_pair(card: Card) -> (usize, u8) {
        (card.suit() as usize, card.rank())
    }

    /// Writes, for opening leads that the robot bid to, the double-dummy
    /// tricks that the defenders take with each lead, on deals of the hidden
    /// hands that fit the auction, one JSON object per line, to the file
    /// `OPENING_LEAD_DATA`.  `OPENING_LEAD_SITUATIONS` (400), `OPENING_LEAD_DEALS`
    /// (6) and `OPENING_LEAD_SEED` (1) say how many and which.  The auction
    /// gives each call's seat counting from the leader: 1 is the dummy, 2
    /// partner and 3 the declarer.  Suits are 0 for clubs to 3 for spades.
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
        let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
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
                            "bids": bids,
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

    fn parse_bid(name: &str) -> Bid {
        let suit = |c: u8| Suit::ALL[b"CDHS".iter().position(|&s| s == c).unwrap()];
        match name {
            "P" => Bid::Pass,
            "X" => Bid::Double,
            "XX" => Bid::Redouble,
            _ => {
                let level = (name.as_bytes()[0] - b'0') as usize;
                match name.as_bytes()[1] {
                    b'N' => Bid::NT(level),
                    c => Bid::Suit(level, suit(c)),
                }
            }
        }
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
                let bids = json["bids"].as_array().unwrap().iter().map(|bid| {
                    (
                        bid[0].as_u64().unwrap() as usize,
                        parse_bid(bid[1].as_str().unwrap()),
                    )
                });
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
                    auction: Auction::from_bids(bids),
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
            let (penalty, held_out) = fitting::cross_validate(&leads, size, &penalties, steps);
            for (penalty, tricks) in held_out {
                println!("  penalty {penalty}: {tricks:.4} tricks held out");
            }
            let weights = fitting::fit(&leads.iter().collect::<Vec<_>>(), size, penalty, steps);
            let test_choices = tuple_choices(&test_leads);
            println!(
                "  best penalty {penalty}: {:.4} tricks on the test leads",
                fitting::mean_value(&weights, &test_choices.iter().collect::<Vec<_>>()),
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
