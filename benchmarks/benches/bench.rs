//! Runs the benchmarks that `bridge-deck` and `bridge-solver` register with
//! their `benchmarks` feature, from both the working tree and the committed
//! main branch.  Each is reported against its own older version.
//!
//! `cargo bench -- "single dummy"` runs only the groups whose names contain
//! "single dummy".

// Linking each crate is what brings in its benchmarks.
use bridge_deck as _;
use bridge_deck_main as _;
use bridge_solver as _;
use bridge_solver_main as _;

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

// Other crates' decks, for scale, in `bridge-deck`'s "deal" group.

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
