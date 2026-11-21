//! Situations in the play of a deal, for the tests and benchmarks of how the
//! robot plays.
//!
//! A situation is a deal that has been bid by the robot to a contract, and
//! then played part of the way by random legal plays, to a point where the
//! player whose turn it is has a choice.  It is always the same one for the
//! same seed, on every platform, so that what is measured is the robot and not
//! the deals.

use crate::{Action, BiddingSystem, BridgeAi, GameState, Seat, Seated};
use bridge_deck::{Card, Cards};
use rand::rngs::StdRng;
use rand::Rng;

/// Makes situations, bidding the auctions with each of the bidding systems in
/// turn.  Making the conventions of a system takes some milliseconds, and the
/// generator keeps them from one situation to the next.
pub(crate) struct Generator {
    tables: Vec<(GameState<()>, BridgeAi)>,
}

impl Generator {
    pub(crate) fn new() -> Generator {
        let tables = BiddingSystem::ALL
            .iter()
            .map(|&system| {
                let mut game = GameState::new();
                game.set_system(system);
                (game, BridgeAi::with_system(system))
            })
            .collect();
        Generator { tables }
    }

    /// The `index`th situation of a stage with `tricks_left` tricks left,
    /// including the trick in progress, as the JSON of its game, or `None`
    /// when the deal was passed out or has nothing to choose.  The system that
    /// bid it is the `index`th system, going round.
    ///
    /// The situations go round the roles that a player can have and the places
    /// in the trick that they can play from (see [`role_and_place`]), so that
    /// sixteen of them in a row are one for each of the sixteen ways to be at
    /// the table.  A stage that cannot have one of those, because the declarer
    /// cannot be on lead with thirteen tricks left, has another in its place.
    pub(crate) fn situation(
        &mut self,
        rng: &mut StdRng,
        index: usize,
        tricks_left: usize,
    ) -> Option<String> {
        let (game, ai) = &mut self.tables[index % BiddingSystem::ALL.len()];
        let mut deck = Cards::ALL;
        let mut deal = || deck.pick_rng(rng, 13).unwrap();
        let hands: Seated<Cards> = [deal(), deal(), deal(), deal()].into();
        game.redeal();
        game.hands = hands;
        game.original_hands = hands;
        game.dealer = Seat::from(rng.gen_range(0..4usize));
        while let Some(seat) = game.bidder() {
            let Action::Bid(bidder, bid) = ai.play(game) else {
                return None;
            };
            if bidder != seat || !game.bid_is_legal(seat, bid) || game.bids.len() > 80 {
                return None;
            }
            game.bids.push(bid);
        }
        let declarer = game.find_declarer()?;
        let wanted = (index % 4, (index + index / 4) % 4);
        let mut found = None;
        // Play from the start of the play as many times as it takes.
        for _ in 0..PLAYS {
            game.hands = game.original_hands;
            game.lead = Some(declarer.next());
            game.played.clear();
            game.ns_tricks = 0;
            game.ew_tricks = 0;
            game.hand_done = false;
            game.shown_out = Seated::new([Cards::EMPTY; 4]);
            game.tricks.clear();
            let cards_before = 4 * (13 - tricks_left) + wanted.1;
            for _ in 0..cards_before {
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
            // There has to be something to choose.
            if game.could_be_played().len() < 2 {
                continue;
            }
            if role_and_place(game) == Some(wanted) {
                return serde_json::to_string(game).ok();
            }
            if found.is_none() {
                found = serde_json::to_string(game).ok();
            }
        }
        found
    }
}

/// How many times to play a deal from the start for a particular role.
const PLAYS: usize = 200;

/// Where the player who is to choose a card sits, as a role and a place in
/// the trick.  The role is 0 for the declarer playing their own hand, 1 for the
/// declarer playing the dummy's, and 2 and 3 for the defenders who sit on
/// the declarer's left, who plays after the dummy, and on their right, who
/// plays before the dummy.  The place is how many cards have been played to
/// the trick, which is 0 for the one on lead.
pub(crate) fn role_and_place<C>(game: &GameState<C>) -> Option<(usize, usize)> {
    let viewer = game.turn()?;
    let mover = game.hand_playing()?;
    let declarer = game.find_declarer()?;
    let role = if viewer == declarer {
        (mover != declarer) as usize
    } else if viewer == declarer.next() {
        2
    } else {
        3
    };
    let place = if game.played.len() == 4 {
        0
    } else {
        game.played.len()
    };
    Some((role, place))
}
