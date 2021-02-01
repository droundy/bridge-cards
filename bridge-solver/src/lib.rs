pub use bridge_deck::{Cards, Card, Suit};

pub fn normalize(all: Cards, hand: Cards) -> Cards {
    let mut c = Cards::EMPTY;
    for &s in [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades].iter() {
        for (n, x) in all.in_suit(s).enumerate() {
            if hand.contains(x) {
                println!("adding {} as {}", x, Card::new(s, 2 + n as u8));
                c = c.insert(Card::new(s, 2 + n as u8));
            }
        }
    }
    c
}

#[test]
fn test_normalize() {
    assert_eq!(Cards::SPADES, normalize(Cards::ALL, Cards::SPADES));
    let twos = Cards::singleton(Card::S2)+Cards::singleton(Card::H2)+Cards::singleton(Card::D2)+Cards::singleton(Card::C2);
    println!("\n\nTwos:\n\n");
    assert_eq!(twos, normalize(Cards::ACES, Cards::ACES));
    println!("\n\nWeird all aces:\n\n");
    assert_eq!(Cards::ACES, normalize(Cards::ALL, Cards::ACES));
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Starting {
    /// The cards that we have, starting with the leader
    pub hands: [Cards; 4],
    /// The cards that I don't know who has
    pub unknown: Cards,
}

#[derive(Default, Clone, Copy, Hash)]
pub struct Score {
    tot_score: u32,
    num: u32,
}

impl Score {
    pub fn mean(self) -> f64 {
        self.tot_score as f64 / self.num as f64
    }
}

impl std::ops::Add<usize> for Score {
    type Output = Score;

    fn add(self, rhs: usize) -> Self::Output {
        Score {
            tot_score: self.tot_score + rhs as u32,
            num: self.num + 1,
        }
    }
}

/// The Naive solver assumes no knowledge from the bidding.
//  It's really only suitable for declarer play when opponents did not bid, and
//  even then is suboptimal on weak partnerships.
#[derive(Clone)]
pub struct Naive {
    cache: std::collections::HashMap<Starting, Score>,
    /// What is trump?
    trump: Option<Suit>,
}

impl Naive {
    fn new(trump: Option<Suit>) -> Self {
        let mut cache = std::collections::HashMap::new();
        // Prepopulate cache with stopping point of no cards->no points.
        cache.insert(
            Starting {
                hands: [Cards::EMPTY; 4],
                unknown: Cards::EMPTY,
            },
            Score::default(),
        );
        Naive { cache, trump }
    }
}

impl Naive {
    pub fn score(&mut self, starting: Starting) -> Score {
        if let Some(score) = self.cache.get(&starting) {
            return *score;
        }
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
