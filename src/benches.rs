//! Benchmarks, registered with `scaling` when the `benchmarks` feature is on.
//!
//! `benchmarks/` in this repository runs them together with the same
//! benchmarks from the main branch, so that each is compared with its older
//! self.  For two versions of this crate to be handed the same input, the
//! inputs are plain integers, which each version turns into its own types
//! with `from_for_bench`.

use crate::{Cards, Suit};
use rand::rngs::{OsRng, SmallRng, StdRng};
use rand::SeedableRng;

impl Cards {
    /// The cards whose bits are set in `bits`: the inverse of
    /// [`Cards::into_for_bench`].
    ///
    /// The encoding must be the same in every version of this crate, since
    /// it is how two versions are handed the same cards.
    pub const fn from_for_bench(bits: u64) -> Cards {
        Cards { bits }
    }

    /// These cards as a `u64`, which [`Cards::from_for_bench`] turns back
    /// into `Cards` in any version of this crate.
    pub const fn into_for_bench(self) -> u64 {
        self.bits
    }
}

impl Suit {
    /// The suit numbered `suit`: the inverse of [`Suit::into_for_bench`].
    ///
    /// The encoding must be the same in every version of this crate, since
    /// it is how two versions are handed the same suit.
    pub fn from_for_bench(suit: u8) -> Suit {
        match suit {
            0 => Suit::Clubs,
            1 => Suit::Diamonds,
            2 => Suit::Hearts,
            3 => Suit::Spades,
            _ => panic!("{suit} is not a suit"),
        }
    }

    /// This suit as a `u8`, which [`Suit::from_for_bench`] turns back into
    /// a `Suit` in any version of this crate.
    pub const fn into_for_bench(self) -> u8 {
        self as u8
    }
}

#[scaling::input(
    group("deal", "deal with StdRng", "deal with SmallRng", "deal with OsRng"),
    sizes(1, 13)
)]
fn cards(n: usize) -> usize {
    n
}

/// The baseline, rather than `cardpack`, which comes first by name.
#[scaling::bench(group = "deal", baseline)]
fn pick(n: &usize) -> Option<Cards> {
    let mut deck = Cards::ALL;
    deck.pick(*n)
}

#[scaling::bench(group = "deal with StdRng")]
fn pick_std_rng(n: &usize) -> impl FnMut() -> Option<Cards> {
    let n = *n;
    let mut rng = StdRng::from_entropy();
    move || {
        let mut deck = Cards::ALL;
        deck.pick_rng(&mut rng, n)
    }
}

#[scaling::bench(group = "deal with SmallRng")]
fn pick_small_rng(n: &usize) -> impl FnMut() -> Option<Cards> {
    let n = *n;
    let mut rng = SmallRng::from_entropy();
    move || {
        let mut deck = Cards::ALL;
        deck.pick_rng(&mut rng, n)
    }
}

#[scaling::bench(group = "deal with OsRng")]
fn pick_os_rng(n: &usize) -> Option<Cards> {
    let mut deck = Cards::ALL;
    deck.pick_rng(&mut OsRng, *n)
}

/// A random 13-card hand.
#[scaling::input(group("hcp", "protected hcp", "lcp", "scp", "hcp in 4+ card suits"))]
fn hand() -> u64 {
    let mut deck = Cards::ALL;
    deck.pick(13).unwrap().into_for_bench()
}

#[scaling::bench(group = "hcp")]
fn high_card_points(hand: &u64) -> usize {
    Cards::from_for_bench(*hand).high_card_points()
}

#[scaling::bench(group = "protected hcp")]
fn protected_high_card_points(hand: &u64) -> usize {
    Cards::from_for_bench(*hand).protected_high_card_points()
}

#[scaling::bench(group = "lcp")]
fn long_card_points(hand: &u64) -> usize {
    Cards::from_for_bench(*hand).long_card_points()
}

#[scaling::bench(group = "scp")]
fn short_card_points(hand: &u64) -> usize {
    Cards::from_for_bench(*hand).short_card_points()
}

#[scaling::bench(group = "hcp in 4+ card suits")]
fn long_suit_high_card_points(hand: &u64) -> usize {
    Cards::from_for_bench(*hand).long_suits(4).high_card_points()
}
