//! Choosing a card by playing the hand out, many times, with the play model.
//!
//! [`PlayoutPlay`] deals the hands that it cannot see again and again, as
//! [`EnsemblePlay`](super::EnsemblePlay) does.  But where that player asks a
//! double-dummy solver what each card is worth in each deal, this one plays
//! the card, and then lets the play model play every seat to the end, each
//! seat choosing from what that seat can see.  So nobody in the playout
//! plays on knowledge of the hidden hands, and the value of a card has none of
//! the bias of "strategy fusion", where a solver that sees all four hands
//! guesses right in each deal and takes tricks that no player could.  The
//! price is that the cards played after the first are the model's, not
//! the best that anyone could find.

use super::ensemble::{trump_of, Hidden};
use super::fitted_play::{model, Model, View};
use super::{PlayAI, RandomPlay};
use crate::{GameState, Seat};
use bridge_deck::{Card, Cards};
use rand::rngs::StdRng;
use rand::SeedableRng;
use std::sync::Arc;

/// How many deals [`PlayoutPlay`] plays out each card in, unless told
/// otherwise.
const DEALS: usize = 100;

/// A player who chooses the card that takes the most tricks on average when
/// the play model plays the rest of the hand, over deals of the hands that
/// cannot be seen which fit the auction and the play so far.
pub struct PlayoutPlay {
    rng: StdRng,
    /// The model that plays the playouts, if it is not the one that the build
    /// includes.
    model: Option<Arc<Model>>,
    deals: usize,
    /// Whether to keep the deals that come closest to being bid as the
    /// auction was.
    replay: bool,
}

impl PlayoutPlay {
    pub fn new() -> Self {
        Self::with_rng(StdRng::from_entropy())
    }

    /// A player whose choices are the same every time.
    pub fn seeded(seed: u64) -> Self {
        Self::with_rng(StdRng::seed_from_u64(seed))
    }

    fn with_rng(rng: StdRng) -> Self {
        PlayoutPlay {
            rng,
            model: None,
            deals: DEALS,
            replay: false,
        }
    }

    /// This player, trying each card in `deals` deals.
    pub fn with_deals(self, deals: usize) -> Self {
        PlayoutPlay { deals, ..self }
    }

    /// This player, with `model` playing the playouts.
    #[cfg(test)]
    pub(crate) fn with_model(self, model: Model) -> Self {
        PlayoutPlay {
            model: Some(Arc::new(model)),
            ..self
        }
    }

    /// This player, dealing only the hands that the robot would have bid the
    /// auction with, or that come closest to it.
    pub fn replaying_the_auction(self) -> Self {
        PlayoutPlay {
            replay: true,
            ..self
        }
    }
}

impl std::fmt::Debug for PlayoutPlay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PlayoutPlay {{ deals: {} }}", self.deals)
    }
}

impl Default for PlayoutPlay {
    fn default() -> Self {
        Self::new()
    }
}

/// A copy of `game` without its connections.
pub(crate) fn copy_of<C>(game: &GameState<C>) -> GameState<()> {
    let mut copy = GameState::new();
    copy.hands = game.hands;
    copy.original_hands = game.original_hands;
    copy.hand_done = game.hand_done;
    copy.dealer = game.dealer;
    copy.bids = game.bids.clone();
    copy.lead = game.lead;
    copy.played = game.played.clone();
    copy.ns_tricks = game.ns_tricks;
    copy.ew_tricks = game.ew_tricks;
    copy.shown_out = game.shown_out;
    copy.tricks = game.tricks.clone();
    copy.system = game.system;
    copy
}

impl PlayAI for PlayoutPlay {
    fn play<C>(&mut self, game: &GameState<C>) -> Card {
        let legal = game.could_be_played();
        if legal.len() == 1 {
            return legal.min().unwrap();
        }
        let own = self.model.clone();
        let Some(model) = (match own.as_deref() {
            Some(own) => Some(own),
            None => model(),
        }) else {
            return RandomPlay.play(game);
        };
        let convention = &**game.system.table();
        let Some(hidden) = Hidden::new(game, convention) else {
            return RandomPlay.play(game);
        };
        if View::new(game, convention).is_none() {
            // The opening lead, which the model chooses as it does.
            return super::fitted_play::model_card(model, game, convention);
        }
        let candidates: Vec<Card> = one_of_each_run(game).collect();
        let deals = hidden.ensemble(&mut self.rng, self.deals, self.replay.then_some(convention));
        let Some(totals) = card_values(game, &candidates, &deals, [model, model], convention) else {
            return RandomPlay.play(game);
        };
        let trump = trump_of(game);
        let cost = |card: Card| card.rank() + if Some(card.suit()) == trump { 13 } else { 0 };
        let best = totals.iter().copied().fold(f64::MIN, f64::max);
        candidates
            .into_iter()
            .zip(totals)
            .filter(|&(_, total)| total == best)
            .map(|(card, _)| card)
            .min_by_key(|&card| cost(card))
            .unwrap()
    }
}

/// The cards that the player to play in `game` could play, but only the
/// lowest of each run of them that touch among the cards not yet played and
/// those of the trick: those win and lose the same tricks, so one stands
/// for all.
pub(crate) fn one_of_each_run<C>(game: &GameState<C>) -> Cards {
    let all = (0..4)
        .map(|s| game.hands[Seat::from(s)])
        .chain(game.played.iter().map(|&card| Cards::singleton(card)))
        .fold(Cards::EMPTY, |all, cards| all + cards);
    game.could_be_played().lowest_of_sequences(all)
}

/// The index in `runs` (see [`one_of_each_run`]) of the run of `card`: the
/// highest card of its suit in `runs` that is not above it.
#[cfg(test)]
pub(crate) fn run_of(runs: &[Card], card: Card) -> usize {
    (0..runs.len())
        .filter(|&i| runs[i].suit() == card.suit() && runs[i].rank() <= card.rank())
        .max_by_key(|&i| runs[i].rank())
        .unwrap()
}

/// The tricks that the side to play in `game` takes on the mean over
/// `deals` of the two hands that the player cannot see (see
/// [`Hidden::ensemble`]), after each of `cards`, when the rest of the hand
/// is played by `models` (north and south's, and east and west's), each
/// card from what its player can see.  Each deal is played out after each
/// card, so that the cards are compared on the same deals.  `None` if
/// there are no deals, or no one is to play.
pub(crate) fn card_values<C>(
    game: &GameState<C>,
    cards: &[Card],
    deals: &[[Cards; 2]],
    models: [&Model; 2],
    convention: &super::Convention,
) -> Option<Vec<f64>> {
    let seats = Hidden::new(game, convention)?.seats();
    let ours = game.turn()? as usize % 2;
    if deals.is_empty() {
        return None;
    }
    let mut totals = vec![0usize; cards.len()];
    for deal in deals {
        let mut base = copy_of(game);
        for (seat, cards) in seats.into_iter().zip(*deal) {
            // What the seat has played is the same in every deal.
            let played = game.original_hands[seat] - game.hands[seat];
            base.hands[seat] = cards;
            base.original_hands[seat] = played + cards;
        }
        for (total, &card) in totals.iter_mut().zip(cards) {
            let mut playout = copy_of(&base);
            playout.play(card);
            finish(&mut playout, models, convention);
            *total += if ours == 0 {
                playout.ns_tricks
            } else {
                playout.ew_tricks
            };
        }
    }
    Some(totals.into_iter().map(|t| t as f64 / deals.len() as f64).collect())
}

/// Plays `game` out to the end, north and south's cards by `models[0]` and
/// east and west's by `models[1]`, each from what the player of it can see.
fn finish(game: &mut GameState<()>, models: [&Model; 2], convention: &super::Convention) {
    while !game.hand_done {
        let legal = game.could_be_played();
        let card = if legal.len() == 1 {
            legal.min().unwrap()
        } else {
            match (View::new(game, convention), game.turn()) {
                (Some(view), Some(seat)) => view.best(models[seat as usize % 2]),
                _ => break,
            }
        };
        game.play(card);
    }
}
