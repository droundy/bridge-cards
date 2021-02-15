use easybench::bench;
use rand::rngs::{OsRng, SmallRng, StdRng};
use rand::{Rng, SeedableRng};

use bridge_deck::{Card, Cards, Suit};
use bridge_solver::{Naive, Starting};

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
        "score last trick NT {}",
        bench(|| Naive::new(None)
            .score(Starting {
                hands: [
                    Cards::singleton(Card::S2),
                    Cards::singleton(Card::D3),
                    Cards::singleton(Card::D4),
                    Cards::singleton(Card::S5),
                ],
                unknown: Cards::EMPTY,
            })
            .mean())
    );
    println!(
        "score last trick spades trump {}",
        bench(|| Naive::new(Some(Suit::Spades))
            .score(Starting {
                hands: [
                    Cards::singleton(Card::S2),
                    Cards::singleton(Card::D3),
                    Cards::singleton(Card::D4),
                    Cards::singleton(Card::S5),
                ],
                unknown: Cards::EMPTY,
            })
            .mean())
    );
    println!(
        "score each player different suit, no trump {}",
        bench(|| Naive::new(None)
            .score(Starting {
                hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
                unknown: Cards::EMPTY,
            })
            .mean())
    );
    println!(
        "score each player different suit, spades trump {}",
        bench(|| Naive::new(Some(Suit::Spades))
            .score(Starting {
                hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
                unknown: Cards::EMPTY,
            })
            .mean())
    );
    println!(
        "score each player different suit, hearts trump {}",
        bench(|| Naive::new(Some(Suit::Hearts))
            .score(Starting {
                hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
                unknown: Cards::EMPTY,
            })
            .mean())
    );
}
