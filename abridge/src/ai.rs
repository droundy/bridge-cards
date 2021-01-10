use crate::Bid;
trait Bidder {
    fn bid(&mut self, history: &[Bid]) -> Bid;
}

struct AllPass;
impl Bidder for AllPass {
    fn bid(&mut self, _history: &[Bid]) -> Bid {
        Bid::Pass
    }
}
