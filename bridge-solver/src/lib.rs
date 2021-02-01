pub use bridge_deck::{Cards, Suit};

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
