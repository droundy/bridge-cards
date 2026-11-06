//! Benchmarks of `bridge_solver`.  Each group compares the working tree
//! (`current`) with the committed main branch (`main`).
//!
//! `cargo bench --bench solver -- "single dummy"` runs only the groups whose
//! names contain "single dummy".
//!
//! The sizes stop where a single call would take most of a second or more:
//! every solver but `oneround` grows exponentially with the number of cards.
//! For the same reason there is no `#[scaling::bench_scaling]` here: it picks
//! its own sizes with no upper limit, extrapolating a power law, and on these
//! costs it walks into calls that take hours.

use bridge_solver::{Card, Cards, Starting, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;

fn main() -> Result<(), scaling::RegistrationError> {
    let filters: Vec<String> = std::env::args()
        .skip(1)
        .filter(|a| !a.starts_with('-'))
        .collect();
    scaling::Config::default()
        .filter_groups(move |group| {
            filters.is_empty() || filters.iter().any(|f| group.contains(f.as_str()))
        })
        .run_and_print()
}

fn suit_on_main(suit: Suit) -> bridge_solver_main::Suit {
    match suit {
        Suit::Clubs => bridge_solver_main::Suit::Clubs,
        Suit::Diamonds => bridge_solver_main::Suit::Diamonds,
        Suit::Hearts => bridge_solver_main::Suit::Hearts,
        Suit::Spades => bridge_solver_main::Suit::Spades,
    }
}

/// The same cards, as the main branch's `Cards`.
fn on_main(cards: Cards) -> bridge_solver_main::Cards {
    cards.fold(bridge_solver_main::Cards::EMPTY, |main, card| {
        main.insert(bridge_solver_main::Card::new(
            suit_on_main(card.suit()),
            card.rank(),
        ))
    })
}

/// A position to score, in one version's types.
#[derive(Clone, Copy)]
struct Problem<S, T> {
    starting: S,
    trump: Option<T>,
}

/// The same position in both versions' types, so that both candidates of a
/// group are handed the same cards.
#[derive(Clone, Copy)]
struct Position {
    main: Problem<bridge_solver_main::Starting, bridge_solver_main::Suit>,
    current: Problem<Starting, Suit>,
}

impl Position {
    fn new(starting: Starting, trump: Option<Suit>) -> Self {
        Position {
            main: Problem {
                starting: bridge_solver_main::Starting {
                    hands: starting.hands.map(on_main),
                    unknown: on_main(starting.unknown),
                },
                trump: trump.map(suit_on_main),
            },
            current: Problem { starting, trump },
        }
    }
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
    Position::new(last_trick(), None)
}

#[scaling::input(group = "last trick", name = "spades", reusable)]
fn last_trick_spades() -> Position {
    Position::new(last_trick(), Some(Suit::Spades))
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
    Position::new(one_suit_each(), None)
}

#[scaling::input(group = "one suit each", name = "spades", reusable)]
fn one_suit_each_spades() -> Position {
    Position::new(one_suit_each(), Some(Suit::Spades))
}

#[scaling::input(group = "one suit each", name = "hearts", reusable)]
fn one_suit_each_hearts() -> Position {
    Position::new(one_suit_each(), Some(Suit::Hearts))
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
    Position::new(single_dummy(n), Some(Suit::Hearts))
}

#[scaling::input(
    group("single dummy, high-low", "single dummy, statistical"),
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6),
    reusable
)]
fn single_dummy_hearts(n: usize) -> Position {
    Position::new(single_dummy(n), Some(Suit::Hearts))
}

#[scaling::input(
    group = "single dummy, no trump",
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6),
    reusable
)]
fn single_dummy_no_trump(n: usize) -> Position {
    Position::new(single_dummy(n), None)
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
    Position::new(full_deal(n), Some(Suit::Hearts))
}

#[scaling::input(
    group = "full deal, no trump",
    name = "cards",
    sizes(1, 2, 3, 4, 5, 6, 7),
    reusable
)]
fn full_deal_no_trump(n: usize) -> Position {
    Position::new(full_deal(n), None)
}

/// Defines every benchmark for one version of `bridge_solver`, as the
/// candidate called `$name` in each group.  Both versions are defined by this
/// one macro so that they cannot drift apart.
macro_rules! benchmarks {
    ($module:ident, $solver:ident, $field:ident, $name:literal $(, $baseline:ident)?) => {
        mod $module {
            use super::*;
            use $solver::Naive;

            /// `Naive::new` is `Naive::statistical` with one sample.
            #[scaling::bench(
                group(
                    "last trick",
                    "one suit each",
                    "single dummy, statistical",
                    "single dummy, no trump",
                    "full deal, hearts",
                    "full deal, no trump"
                ),
                name = $name
                $(, $baseline)?
            )]
            fn naive(position: &Position) -> f64 {
                let p = position.$field;
                Naive::new(p.trump).score(p.starting).mean()
            }

            #[scaling::bench(group = "single dummy, oneround", name = $name $(, $baseline)?)]
            fn oneround(position: &Position) -> f64 {
                let p = position.$field;
                Naive::oneround(p.trump).score(p.starting).mean()
            }

            #[scaling::bench(group = "single dummy, high-low", name = $name $(, $baseline)?)]
            fn high_low(position: &Position) -> f64 {
                let p = position.$field;
                Naive::high_low(p.trump).score(p.starting).mean()
            }
        }
    };
}

benchmarks!(main_branch, bridge_solver_main, main, "main", baseline);
benchmarks!(working_tree, bridge_solver, current, "current");
