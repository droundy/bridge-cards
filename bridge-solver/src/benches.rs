//! Benchmarks, registered with `scaling` when the `benchmarks` feature is on.
//!
//! `benchmarks/` in this repository runs them together with the same
//! benchmarks from the main branch, so that each is compared with its older
//! self.  For two versions of this crate to be handed the same position, the
//! inputs are plain integers, which each version turns into its own types
//! with `from_for_bench`.
//!
//! The sizes stop where a single call would take most of a second or more:
//! every solver but `oneround` grows exponentially with the number of cards.
//! For the same reason there is no `#[scaling::bench_scaling]` here: it picks
//! its own sizes with no upper limit, extrapolating a power law, and on these
//! costs it walks into calls that take hours.

use crate::{Card, Cards, Naive, Starting, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;

impl Starting {
    /// The position whose hands and unknown cards are encoded as
    /// [`Cards::from_for_bench`] takes them: the inverse of
    /// [`Starting::into_for_bench`].
    pub fn from_for_bench((hands, unknown): ([u64; 4], u64)) -> Starting {
        Starting {
            hands: hands.map(Cards::from_for_bench),
            unknown: Cards::from_for_bench(unknown),
        }
    }

    /// This position in plain integers, which [`Starting::from_for_bench`]
    /// turns back into a `Starting` in any version of this crate.
    pub fn into_for_bench(self) -> ([u64; 4], u64) {
        (
            self.hands.map(Cards::into_for_bench),
            self.unknown.into_for_bench(),
        )
    }
}

/// A position to score and its trump suit, as `from_for_bench` takes them.
type Position = (([u64; 4], u64), Option<u8>);

fn position(starting: Starting, trump: Option<Suit>) -> Position {
    (starting.into_for_bench(), trump.map(Suit::into_for_bench))
}

fn from_position(&(starting, trump): &Position) -> (Starting, Option<Suit>) {
    (
        Starting::from_for_bench(starting),
        trump.map(Suit::from_for_bench),
    )
}

fn last_trick() -> Starting {
    Starting {
        hands: [
            Cards::singleton(Card::S2),
            Cards::singleton(Card::D3),
            Cards::singleton(Card::D4),
            Cards::singleton(Card::S5),
        ],
        unknown: Cards::EMPTY,
    }
}

#[scaling::input(group = "last trick", name = "no trump", reusable)]
fn last_trick_no_trump() -> Position {
    position(last_trick(), None)
}

#[scaling::input(group = "last trick", name = "spades", reusable)]
fn last_trick_spades() -> Position {
    position(last_trick(), Some(Suit::Spades))
}

/// Each player holds a whole suit.
fn one_suit_each() -> Starting {
    Starting {
        hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS],
        unknown: Cards::EMPTY,
    }
}

#[scaling::input(group = "one suit each", name = "no trump", reusable)]
fn one_suit_each_no_trump() -> Position {
    position(one_suit_each(), None)
}

#[scaling::input(group = "one suit each", name = "spades", reusable)]
fn one_suit_each_spades() -> Position {
    position(one_suit_each(), Some(Suit::Spades))
}

#[scaling::input(group = "one suit each", name = "hearts", reusable)]
fn one_suit_each_hearts() -> Position {
    position(one_suit_each(), Some(Suit::Hearts))
}

/// Declarer and dummy (seats 1 and 3) hold `n` cards each, and the defenders'
/// `2n` cards are unknown.
fn single_dummy(n: usize) -> Starting {
    let mut rng = SmallRng::seed_from_u64(3);
    let mut cards = Cards::ALL;
    let mut hands = [Cards::EMPTY; 4];
    for i in [1, 3] {
        hands[i] = cards.pick_rng(&mut rng, n).unwrap();
    }
    let unknown = cards.pick_rng(&mut rng, 2 * n).unwrap();
    Starting { hands, unknown }
}

#[scaling::input(
    group = "single dummy, oneround",
    name = "cards",
    sizes(1, 4, 7, 10, 13),
    reusable
)]
fn single_dummy_oneround(n: usize) -> Position {
    position(single_dummy(n), Some(Suit::Hearts))
}

#[scaling::input(
    group("single dummy, high-low", "single dummy, statistical"),
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6),
    reusable
)]
fn single_dummy_hearts(n: usize) -> Position {
    position(single_dummy(n), Some(Suit::Hearts))
}

#[scaling::input(
    group = "single dummy, no trump",
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6),
    reusable
)]
fn single_dummy_no_trump(n: usize) -> Position {
    position(single_dummy(n), None)
}

/// All four players hold `n` cards, and nothing is unknown.
fn full_deal(n: usize) -> Starting {
    let mut rng = SmallRng::seed_from_u64(3);
    let mut cards = Cards::ALL;
    let mut hands = [Cards::EMPTY; 4];
    for hand in &mut hands {
        *hand = cards.pick_rng(&mut rng, n).unwrap();
    }
    Starting {
        hands,
        unknown: Cards::EMPTY,
    }
}

#[scaling::input(
    group = "full deal, hearts",
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6, 7),
    reusable
)]
fn full_deal_hearts(n: usize) -> Position {
    position(full_deal(n), Some(Suit::Hearts))
}

#[scaling::input(
    group = "full deal, no trump",
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6, 7),
    reusable
)]
fn full_deal_no_trump(n: usize) -> Position {
    position(full_deal(n), None)
}

/// `Naive::new` is `Naive::statistical` with one sample.
#[scaling::bench(group(
    "last trick",
    "one suit each",
    "single dummy, statistical",
    "single dummy, no trump",
    "full deal, hearts",
    "full deal, no trump"
))]
fn naive(position: &Position) -> f64 {
    let (starting, trump) = from_position(position);
    Naive::new(trump).score(starting).mean()
}

#[scaling::bench(group = "single dummy, oneround")]
fn oneround(position: &Position) -> f64 {
    let (starting, trump) = from_position(position);
    Naive::oneround(trump).score(starting).mean()
}

#[scaling::bench(group = "single dummy, high-low")]
fn high_low(position: &Position) -> f64 {
    let (starting, trump) = from_position(position);
    Naive::high_low(trump).score(starting).mean()
}
