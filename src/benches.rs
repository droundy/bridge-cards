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
    Cards::from_for_bench(*hand)
        .long_suits(4)
        .high_card_points()
}

/// 256 random 13-card hands, so that what a method does cannot be
/// predicted from one hand to the next.  Each benchmark below adds up its
/// method's answer over all of them.
#[scaling::input(group(
    "len",
    "in_suits",
    "high_card_points",
    "protected_high_card_points",
    "long_suits",
    "long_card_points",
    "short_card_points",
    "new_losing_trick_count",
    "values",
    "iterate",
    "min and max",
    "card suit and rank",
    "display",
    "parse",
    "all point counts",
))]
fn hands() -> Vec<u64> {
    (0..256)
        .map(|_| {
            let mut deck = Cards::ALL;
            deck.pick(13).unwrap().into_for_bench()
        })
        .collect()
}

fn sum_over(hands: &[u64], f: impl Fn(Cards) -> usize) -> usize {
    hands
        .iter()
        .map(|&h| f(Cards::from_for_bench(h)))
        .fold(0, usize::wrapping_add)
}

#[scaling::bench(group = "len", baseline)]
fn many_len(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.len())
}

#[scaling::bench(group = "in_suits")]
fn many_in_suits(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.in_suits().map(|s| s.len()).sum())
}

#[scaling::bench(group = "high_card_points")]
fn many_high_card_points(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.high_card_points())
}

#[scaling::bench(group = "protected_high_card_points")]
fn many_protected_high_card_points(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.protected_high_card_points())
}

#[scaling::bench(group = "long_suits")]
fn many_long_suits(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.long_suits(4).len())
}

#[scaling::bench(group = "long_card_points")]
fn many_long_card_points(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.long_card_points())
}

#[scaling::bench(group = "short_card_points")]
fn many_short_card_points(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.short_card_points())
}

#[scaling::bench(group = "new_losing_trick_count")]
fn many_new_losing_trick_count(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| (h.new_losing_trick_count() * 2.0) as usize)
}

#[scaling::bench(group = "values")]
fn many_values(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| {
        let v = h.values();
        v.hcp as usize + v.lhcp as usize + v.shcp as usize + v.nltc2 as usize
    })
}

#[scaling::bench(group = "iterate")]
fn many_iterate(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.map(|c| c.rank() as usize).sum())
}

#[scaling::bench(group = "min and max")]
fn many_min_and_max(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| {
        h.min().unwrap().rank() as usize + h.max().unwrap().rank() as usize
    })
}

#[scaling::bench(group = "card suit and rank")]
fn many_card_suit_and_rank(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| {
        h.map(|c| c.suit() as usize * 16 + c.rank() as usize).sum()
    })
}

#[scaling::bench(group = "display")]
fn many_display(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| h.to_string().len())
}

#[scaling::bench(group = "parse")]
fn many_parse(hands: &Vec<u64>) -> usize {
    // Parsing is timed on its own: the strings are made once for each call.
    let strings: Vec<String> = hands
        .iter()
        .map(|&h| Cards::from_for_bench(h).to_string())
        .collect();
    strings
        .iter()
        .map(|s| s.parse::<Cards>().unwrap().len())
        .sum()
}

/// What `values()` needs from a hand, without its other fields: the four
/// point counts that are each their own method.
#[scaling::bench(group = "all point counts")]
fn many_all_point_counts(hands: &Vec<u64>) -> usize {
    sum_over(hands, |h| {
        h.high_card_points()
            + h.protected_high_card_points()
            + h.long_card_points()
            + h.short_card_points()
    })
}
