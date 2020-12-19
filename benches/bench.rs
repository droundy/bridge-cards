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
    println!();
    println!();
    println!(
        "count hcp in 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().high_card_points())
    );
    println!(
        "count hcp in 4+ card suits of 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().long_suits(4).high_card_points())
    );
    println!();
}
