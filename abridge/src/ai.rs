use crate::{Bid, GameState};
use bridge_deck::Card;
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
