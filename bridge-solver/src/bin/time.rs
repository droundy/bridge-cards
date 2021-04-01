use rand::rngs::SmallRng;
use rand::SeedableRng;

use bridge_deck::{Cards, Suit};
use bridge_solver::{Naive, Starting};

fn main() {
    fn gen_starting(n: usize) -> Starting {
        let mut rng = SmallRng::seed_from_u64(3);
        let mut cards = Cards::ALL;
        let mut hands = [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS];
        for i in 0..4 {
            hands[i] = cards.pick_rng(&mut rng, n).unwrap();
        }
        Starting {
            hands,
            unknown: Cards::EMPTY,
        }
    }

    for n in 1..12 {
        println!(
            "No trump {}: {:?}",
            n,
            Naive::new(Some(Suit::Hearts)).score(gen_starting(n)).mean()
        );
    }
}
