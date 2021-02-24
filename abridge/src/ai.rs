use crate::{Bid, GameState};
use bridge_deck::{Card, Cards};
pub trait BidAI: std::fmt::Debug {
    fn bid(&mut self, history: &[Bid]) -> Bid;
}
pub trait PlayAI: std::fmt::Debug {
    fn play(&mut self, game: &GameState) -> Card;
}

#[derive(Debug)]
pub struct AllPass;
impl BidAI for AllPass {
    fn bid(&mut self, _history: &[Bid]) -> Bid {
        Bid::Pass
    }
}

#[derive(Debug)]
pub struct RandomPlay;
impl PlayAI for RandomPlay {
    fn play(&mut self, game: &GameState) -> Card {
        game.could_be_played().pick(1).unwrap().next().unwrap()
    }
}

pub trait BiddingConvention: Send + Sync {
    /// Does this convention apply to the last bid in this sequence?
    fn applies(&self, bids: &[Bid]) -> bool;
    /// A short description of the meaning of this bid.
    fn description(&self, bids: &[Bid]) -> String;
    /// Name of the convention
    fn name(&self) -> String;
    /// Does this bid work for this hand?
    fn is_appropriate(&self, bids: &[Bid], hand: Cards) -> bool;

    /// A convenient version of description
    fn bid_description(&self, bid: Bid, old_bids: &[Bid]) -> String {
        let mut bids = old_bids.iter().cloned().collect::<Vec<_>>();
        bids.push(bid);
        self.description(&bids)
    }
}

#[derive(Clone)]
pub struct SimpleConvention {
    pub bids: &'static [&'static [Bid]],
    pub the_description: &'static str,
    pub the_name: &'static str,
}

impl BiddingConvention for SimpleConvention {
    fn applies(&self, bids: &[Bid]) -> bool {
        self.bids.contains(&bids)
    }

    fn description(&self, _bids: &[Bid]) -> String {
        self.the_description.to_string()
    }

    fn name(&self) -> String {
        self.the_name.to_string()
    }

    fn is_appropriate(&self, _bids: &[Bid], _hand: Cards) -> bool {
        false
    }
}

#[derive(Default)]
pub struct OrderedConventions {
    the_name: &'static str,
    conventions: Vec<std::sync::Arc<dyn BiddingConvention>>,
}

impl OrderedConventions {
    /// A new convention
    pub fn new(name: &'static str) -> Self {
        OrderedConventions {
            the_name: name,
            conventions: Vec::new(),
        }
    }
    /// Add another possibility to the OrderedConvention
    pub fn add<T: BiddingConvention + 'static>(&mut self, convention: T) {
        self.conventions.push(std::sync::Arc::new(convention));
    }
}

impl BiddingConvention for OrderedConventions {
    fn applies(&self, bids: &[Bid]) -> bool {
        self.conventions.iter().any(|c| c.applies(bids))
    }

    fn description(&self, bids: &[Bid]) -> String {
        if let Some(c) = self.conventions.iter().filter(|c| c.applies(bids)).next() {
            c.description(bids)
        } else {
            "?".to_string()
        }
    }

    fn name(&self) -> String {
        self.the_name.to_string()
    }

    fn is_appropriate(&self, bids: &[Bid], hand: Cards) -> bool {
        if let Some(c) = self.conventions.iter().filter(|c| c.applies(bids)).next() {
            c.is_appropriate(bids, hand)
        } else {
            false
        }
    }
}

impl OrderedConventions {
    fn sheets() -> Self {
        let mut sheets = OrderedConventions::new("Sheets");
        use bridge_deck::Suit::*;
        use Bid::*;
        sheets.add(SimpleConvention {
            the_name: "Opening bid",
            bids: &[
                &[Suit(1, Hearts)],
                &[Pass, Suit(1, Hearts)],
                &[Pass, Pass, Suit(1, Hearts)],
                &[Pass, Pass, Pass, Suit(1, Hearts)],
            ],
            the_description: "13+ lhcp, ♥≥5, ♥≥♠",
        });
        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(1, Spades)],
                &[Pass, Suit(1, Spades)],
                &[Pass, Pass, Suit(1, Spades)],
                &[Pass, Pass, Pass, Suit(1, Spades)],
            ],
            the_description: "13+ lhcp, ♠≥5, ♠>♥",
            the_name: "Opening bid",
        });
        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(1, Clubs)],
                &[Pass, Suit(1, Clubs)],
                &[Pass, Pass, Suit(1, Clubs)],
                &[Pass, Pass, Pass, Suit(1, Clubs)],
            ],
            the_description: "13+ lhcp, ♣≥3, ♣≥♦ ♥<5, ♠≥5",
            the_name: "Opening bid",
        });
        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(1, Diamonds)],
                &[Pass, Suit(1, Diamonds)],
                &[Pass, Pass, Suit(1, Diamonds)],
                &[Pass, Pass, Pass, Suit(1, Diamonds)],
            ],
            the_description: "13+ lhcp, ♦≥3, ♦>♣, ♥<5, ♠≥5",
            the_name: "Opening bid",
        });

        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(2, Hearts)],
                &[Pass, Suit(2, Hearts)],
                &[Pass, Pass, Suit(2, Hearts)],
            ],
            the_description: "5-10 hcp, 6♥",
            the_name: "Weak two",
        });
        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(2, Spades)],
                &[Pass, Suit(2, Spades)],
                &[Pass, Pass, Suit(2, Spades)],
            ],
            the_description: "5-10 hcp, 6♠",
            the_name: "Weak two",
        });
        sheets.add(SimpleConvention {
            bids: &[
                &[Suit(2, Diamonds)],
                &[Pass, Suit(2, Diamonds)],
                &[Pass, Pass, Suit(2, Diamonds)],
            ],
            the_description: "5-10 hcp, 6♦",
            the_name: "Weak two",
        });

        sheets.add(SimpleConvention {
            bids: &[
                &[Pass],
                &[Pass, Pass],
                &[Pass, Pass, Pass],
                &[Pass, Pass, Pass, Pass],
            ],
            the_description: "Less than 13 lhcp",
            the_name: "Opening pass",
        });
        sheets
    }
}
