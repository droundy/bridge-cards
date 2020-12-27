use easybench::{bench};
use rand::{SeedableRng, Rng};
use rand::rngs::{StdRng, OsRng, SmallRng};

fn main() {
    println!(
        "deal 13 cards {}",
        bench(|| bridge_deck::Cards::ALL.pick(13))
    );
    println!(
        "deal 13 cards {}",
        bench(|| bridge_deck::Cards::ALL.pick(13))
    );
    let mut std_rng = StdRng::from_entropy();
    let mut small_rng = SmallRng::from_entropy();
    let mut os_rng = OsRng::default();
        println!(
        "deal 13 cards with std rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut std_rng, 13))
    );
    let mut os_rng = OsRng::default();
        println!(
        "deal 13 cards with small rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut small_rng, 13))
    );
    println!(
        "deal 13 cards with os rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut os_rng, 13))
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
        "deal 1 card {}",
        bench(|| bridge_deck::Cards::ALL.pick(1))
    );
    let mut os_rng = OsRng::default();
        println!(
        "deal 1 card with std rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut std_rng, 1))
    );
    let mut os_rng = OsRng::default();
        println!(
        "deal 1 card with small rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut small_rng, 1))
    );
    println!(
        "deal 1 card with os rng {}",
        bench(|| bridge_deck::Cards::ALL.pick_rng(&mut os_rng, 1))
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
        "count protected hcp in 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().high_card_points())
    );
    println!(
        "count lcp in 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().long_card_points())
    );
    println!(
        "count scp in 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().short_card_points())
    );
    println!(
        "count hcp in 4+ card suits of 13 card hands {}",
        bench(|| bridge_deck::Cards::ALL.pick(13).unwrap().long_suits(4).high_card_points())
    );
    println!();
}
