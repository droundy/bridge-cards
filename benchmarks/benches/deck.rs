//! Benchmarks of `bridge_deck`.  Each group compares the working tree
//! (`current`) with the committed main branch (`main`).
//!
//! `cargo bench --bench deck -- deal` runs only the groups whose names
//! contain "deal".

use rand::rngs::{OsRng, SmallRng, StdRng};
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

/// The same cards, as the main branch's `Cards`.
fn on_main(cards: bridge_deck::Cards) -> bridge_deck_main::Cards {
    use bridge_deck::Suit::*;
    cards.fold(bridge_deck_main::Cards::EMPTY, |main, card| {
        let suit = match card.suit() {
            Clubs => bridge_deck_main::Suit::Clubs,
            Diamonds => bridge_deck_main::Suit::Diamonds,
            Hearts => bridge_deck_main::Suit::Hearts,
            Spades => bridge_deck_main::Suit::Spades,
        };
        main.insert(bridge_deck_main::Card::new(suit, card.rank()))
    })
}

#[scaling::input(
    group("deal", "deal with StdRng", "deal with SmallRng", "deal with OsRng"),
    sizes(1, 13)
)]
fn cards(n: usize) -> usize {
    n
}

/// A random 13-card hand in both versions' types, so that both candidates of
/// a group are handed the same hand.
#[derive(Clone, Copy)]
struct Hand {
    main: bridge_deck_main::Cards,
    current: bridge_deck::Cards,
}

#[scaling::input(group("hcp", "protected hcp", "lcp", "scp", "hcp in 4+ card suits"))]
fn hand() -> Hand {
    let mut deck = bridge_deck::Cards::ALL;
    let current = deck.pick(13).unwrap();
    Hand {
        main: on_main(current),
        current,
    }
}

/// Defines every benchmark for one version of `bridge_deck`, as the candidate
/// called `$name` in each group.  Both versions are defined by this one macro
/// so that they cannot drift apart.
macro_rules! benchmarks {
    ($module:ident, $deck:ident, $field:ident, $name:literal $(, $baseline:ident)?) => {
        mod $module {
            use super::*;
            use $deck::Cards;

            #[scaling::bench(group = "deal", name = $name $(, $baseline)?)]
            fn deal(n: &usize) -> Option<Cards> {
                let mut deck = Cards::ALL;
                deck.pick(*n)
            }

            #[scaling::bench(group = "deal with StdRng", name = $name $(, $baseline)?)]
            fn deal_std_rng(n: &usize) -> impl FnMut() -> Option<Cards> {
                let n = *n;
                let mut rng = StdRng::from_entropy();
                move || {
                    let mut deck = Cards::ALL;
                    deck.pick_rng(&mut rng, n)
                }
            }

            #[scaling::bench(group = "deal with SmallRng", name = $name $(, $baseline)?)]
            fn deal_small_rng(n: &usize) -> impl FnMut() -> Option<Cards> {
                let n = *n;
                let mut rng = SmallRng::from_entropy();
                move || {
                    let mut deck = Cards::ALL;
                    deck.pick_rng(&mut rng, n)
                }
            }

            #[scaling::bench(group = "deal with OsRng", name = $name $(, $baseline)?)]
            fn deal_os_rng(n: &usize) -> Option<Cards> {
                let mut deck = Cards::ALL;
                deck.pick_rng(&mut OsRng, *n)
            }

            #[scaling::bench(group = "hcp", name = $name $(, $baseline)?)]
            fn hcp(hand: &Hand) -> usize {
                hand.$field.high_card_points()
            }

            #[scaling::bench(group = "protected hcp", name = $name $(, $baseline)?)]
            fn protected_hcp(hand: &Hand) -> usize {
                hand.$field.protected_high_card_points()
            }

            #[scaling::bench(group = "lcp", name = $name $(, $baseline)?)]
            fn lcp(hand: &Hand) -> usize {
                hand.$field.long_card_points()
            }

            #[scaling::bench(group = "scp", name = $name $(, $baseline)?)]
            fn scp(hand: &Hand) -> usize {
                hand.$field.short_card_points()
            }

            #[scaling::bench(group = "hcp in 4+ card suits", name = $name $(, $baseline)?)]
            fn long_suit_hcp(hand: &Hand) -> usize {
                hand.$field.long_suits(4).high_card_points()
            }
        }
    };
}

benchmarks!(main_branch, bridge_deck_main, main, "main", baseline);
benchmarks!(working_tree, bridge_deck, current, "current");

// Other crates' decks, for scale.

#[scaling::bench(group = "deal", name = "cardpack", uninteresting)]
fn deal_cardpack(n: &usize) -> Option<cardpack::Pile> {
    cardpack::Pack::spades_deck().cards().shuffle().draw(*n)
}

#[scaling::bench(group = "deal", name = "simple_cards", uninteresting)]
fn deal_simple_cards(n: &usize) -> simple_cards::Deck {
    let mut deck = simple_cards::Deck::default();
    for _ in 0..*n {
        deck.draw();
    }
    deck
}
