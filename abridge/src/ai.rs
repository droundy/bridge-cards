use crate::{Bid, GameState};
use bridge_deck::Card;
trait BidAI {
    fn bid(&mut self, history: &[Bid]) -> Bid;
}
trait PlayAI {
    fn play(&mut self, game: &GameState) -> Card;
}

struct AllPass;
impl BidAI for AllPass {
    fn bid(&mut self, _history: &[Bid]) -> Bid {
        Bid::Pass
    }
}

struct RandomPlay;
impl PlayAI for RandomPlay {
    fn play(&mut self, game: &GameState) -> Card {
        game.could_be_played().pick(1).unwrap().next().unwrap()
    }
}