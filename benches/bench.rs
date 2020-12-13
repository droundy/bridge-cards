use easybench::{bench};

fn main() {
    println!(
        "deal 13 cards {}",
        bench(|| bridge_deck::Cards::ALL.pick(13))
    );
    println!(
        "deal 13 cards (cardpack) {}",
        bench(|| cardpack::Pack::spades_deck().cards().shuffle().draw(13))
    );
    println!();
    println!(
        "deal 1 card {}",
        bench(|| bridge_deck::Cards::ALL.pick(1))
    );
    println!(
        "deal 1 card (cardpack) {}",
        bench(|| cardpack::Pack::spades_deck().cards().shuffle().draw(1))
    );
    println!(
        "deal 1 cards (simple_cards) {}",
        bench(|| simple_cards::Deck::default().draw())
    );
    println!();
}
