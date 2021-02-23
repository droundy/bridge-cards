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
}

#[derive(Clone)]
pub struct SimpleConvention {
    pub bids: &'static [Bid],
    pub the_description: &'static str,
    pub the_name: &'static str,
}

impl BiddingConvention for SimpleConvention {
    fn applies(&self, bids: &[Bid]) -> bool {
        self.bids == bids
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
            "Does not apply".to_string()
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
