use rand::rngs::{SmallRng, StdRng};
use rand::SeedableRng;
use scaling::{bench, bench_scaling_gen};

use bridge_deck::{Card, Cards, Suit};
use bridge_solver::{Naive, Starting};

fn main() {
    println!(
        "deal 13 cards {}",
        bench(|| bridge_deck::Cards::ALL.clone().pick(13))
    );
    let mut std_rng = StdRng::from_entropy();
    let mut small_rng = SmallRng::from_entropy();
    println!(
        "deal 13 cards with std rng {}",
        bench(|| bridge_deck::Cards::ALL.clone().pick_rng(&mut std_rng, 13))
    );
    println!(
        "deal 13 cards with small rng {}",
        bench(|| bridge_deck::Cards::ALL.clone().pick_rng(&mut small_rng, 13))
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

    let mut cards = Cards::ALL;
    for _ in 0..4 {
        println!("   \"{}\".parse().unwrap(),", cards.pick(13).unwrap());
    }

    fn gen_starting_single_dummy(n: usize) -> Starting {
        let mut rng = SmallRng::seed_from_u64(3);
        let mut cards = Cards::ALL;
        let mut hands = [Cards::EMPTY; 4];
        let mut unknown = Cards::EMPTY;
        for i in [1, 3].iter().cloned() {
            hands[i] = cards.pick_rng(&mut rng, n).unwrap();
        }
        unknown += cards.pick_rng(&mut rng, 2 * n).unwrap();

        Starting { hands, unknown }
    }
    for n in 1..14 {
        println!("{} cards declarer", n);
        println!(
            "     trump        {}",
            bench(|| Naive::new(Some(Suit::Hearts))
                .score(gen_starting_single_dummy(n))
                .mean())
        );
        println!(
            "    oneround      {}",
            bench(|| Naive::oneround(Some(Suit::Hearts))
                .score(gen_starting_single_dummy(n))
                .mean())
        );
        println!(
            "    high-low      {}",
            bench(|| Naive::high_low(Some(Suit::Hearts))
                .score(gen_starting_single_dummy(n))
                .mean())
        );
        println!(
            "    statistical 8 {}",
            bench(|| Naive::statistical(Some(Suit::Hearts), 8)
                .score(gen_starting_single_dummy(n))
                .mean())
        );
        if n < 8 {
            println!(
                "    statistical 256 {}",
                bench(|| Naive::statistical(Some(Suit::Hearts), 256)
                    .score(gen_starting_single_dummy(n))
                    .mean())
            );
        }
        println!();
        println!(
            "    no trump      {}",
            bench(|| Naive::new(None).score(gen_starting_single_dummy(n)).mean())
        );
        println!();
    }

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

    let one_hand = Cards::ALL.clone().pick(13).unwrap();
    let opening_lead = Starting {
        hands: [one_hand, Cards::EMPTY, Cards::EMPTY, Cards::EMPTY],
        unknown: Cards::ALL - one_hand,
    };
    let mut dummy_after_opening_lead = opening_lead.clone();
    dummy_after_opening_lead.hands[1] = dummy_after_opening_lead.unknown.pick(13).unwrap();
    let dummy_after_opening_lead_play = one_hand.clone().pick(1).unwrap().next().unwrap();
    println!(
        "next after opening lead, trump    {}",
        bench(|| Naive::new(Some(Suit::Hearts))
            .score_after(dummy_after_opening_lead, &[dummy_after_opening_lead_play])
            .0
            .mean())
    );
    println!(
        "next after opening lead, no trump {}",
        bench(|| Naive::new(None)
            .score_after(dummy_after_opening_lead, &[dummy_after_opening_lead_play])
            .0
            .mean())
    );
    println!(
        "opening lead cards, trump    {}",
        bench(|| Naive::new(Some(Suit::Hearts)).score(opening_lead).mean())
    );
    println!(
        "opening lead cards, no trump {}",
        bench(|| Naive::new(None).score(opening_lead).mean())
    );
    for n in 1..14 {
        println!(
            "{} cards, trump    {}",
            n,
            bench(|| Naive::new(Some(Suit::Hearts)).score(gen_starting(n)).mean())
        );
        println!(
            "{} cards, no trump {}",
            n,
            bench(|| Naive::new(None).score(gen_starting(n)).mean())
        );
    }

    println!(
        "trump scaling with deal size {}",
        bench_scaling_gen(
            gen_starting,
            |starting| Naive::new(Some(Suit::Hearts)).score(*starting).mean(),
            0
        )
    );
    println!(
        "no trump scaling with deal size {}",
        bench_scaling_gen(
            gen_starting,
            |starting| Naive::new(None).score(*starting).mean(),
            0
        )
    );
}
