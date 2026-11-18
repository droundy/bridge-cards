//! Benchmarks of how the robot chooses a card to play, registered with
//! `scaling` when the `benchmarks` feature is on.
//!
//! A benchmark here is a stage of the play, which is how many tricks are left,
//! with some situations at that stage.  A situation is a deal that has been
//! played part of the way through by random legal plays, and has a robot to
//! choose a card.  It is kept as the JSON that the browser hands to
//! [`choose_play`](crate::choose_play), so that two versions of this crate are
//! handed the same situations and the same function.  The same situations are
//! made every time, so that what is measured is the robot and not the deals.
//!
//! Each benchmark reports how long the robot takes to choose a card in every
//! situation of a stage, and how well it chose, as the double-dummy loss: the
//! tricks that the best play would win, less those that the card that it
//! chose would, when the cards in all four hands are known and everyone plays
//! the best they can.
//!
//! The double-dummy loss is a yardstick and not a measure of how well the
//! robot plays.  The robot cannot see the hidden hands, so the best card for
//! the deal that is really there is often not a card that anyone could know
//! to play, and the robot loses something even if it plays as well as can be.
//! The best play when the hands are known is the same for every version of the
//! robot, so what is meaningful is the difference between versions, and
//! between the robot and the random play that the group also has for scale.
//! Working out the best play gets too slow with more than
//! [`DOUBLE_DUMMY_TRICKS`] tricks left, and the earlier stages report no
//! loss.  The robot samples deals for the cards that it cannot see, so its
//! choices vary from run to run, and so does the loss, which is a mean over
//! the situations of a stage.

use crate::{Action, Bid, GameState, Seat, Seated};
use bridge_deck::{Card, Cards, Suit};
use bridge_solver::{Naive, Starting};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use scaling::Metrics;

/// How many situations make up each stage by default, which is few enough for
/// a stage to be timed in a reasonable time.
const SITUATIONS: usize = 8;

/// How many situations make up each stage: [`SITUATIONS`], or the number in
/// the environment variable `PLAY_BENCH_SITUATIONS`.  The situations of a
/// smaller number are the first of those of a larger.  A mean over only a few
/// situations says little about how well the robot chose, since most
/// of the choices lose nothing, so a run to compare the quality of two
/// versions wants many.
fn situations_per_stage() -> usize {
    std::env::var("PLAY_BENCH_SITUATIONS")
        .ok()
        .and_then(|n| n.parse().ok())
        .unwrap_or(SITUATIONS)
}

/// The most tricks left for which the best play is worked out.
const DOUBLE_DUMMY_TRICKS: usize = 8;

/// A situation with `tricks_left` tricks left, including the one in progress,
/// as the JSON that the browser hands to `choose_play`.  `game` is any game,
/// since making one takes a few milliseconds that we do not want to repeat
/// for every situation.
fn situation(game: &mut GameState<()>, rng: &mut StdRng, tricks_left: usize) -> Option<String> {
    let mut deck = Cards::ALL;
    let mut deal = || deck.pick_rng(rng, 13).unwrap();
    let hands: Seated<Cards> = [deal(), deal(), deal(), deal()].into();
    game.redeal();
    game.hands = hands;
    game.original_hands = hands;
    // The contract is a game, bid by the declarer and passed out.
    let declarer: Seat = rng.gen_range(0..4usize).into();
    let contract = match rng.gen_range(0..5) {
        4 => Bid::NT(3),
        n => Bid::Suit(4, Suit::ALL[n]),
    };
    game.dealer = declarer;
    game.bids = vec![contract, Bid::Pass, Bid::Pass, Bid::Pass];
    game.lead = Some(declarer.next());
    // Play to some point in a trick.
    let cards_before = 4 * (13 - tricks_left) + rng.gen_range(0..4);
    for _ in 0..cards_before {
        let legal: Vec<Card> = game.could_be_played().collect();
        game.play(legal[rng.gen_range(0..legal.len())]);
    }
    // There has to be something to choose.
    if game.could_be_played().len() < 2 {
        return None;
    }
    serde_json::to_string(game).ok()
}

/// Situations with `tricks_left` tricks left, which are the same every time.
pub fn play_situations(tricks_left: usize) -> Vec<String> {
    let mut game = GameState::<()>::new();
    let mut rng = StdRng::seed_from_u64(tricks_left as u64);
    let mut found = Vec::new();
    let wanted = situations_per_stage();
    while found.len() < wanted {
        found.extend(situation(&mut game, &mut rng, tricks_left));
    }
    found
}

#[scaling::input(
    group = "play decision",
    name = "tricks left",
    sizes(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2),
    reusable
)]
fn play_decision_situations(tricks_left: usize) -> Vec<String> {
    play_situations(tricks_left)
}

/// What the browser does when it is the robot's turn to play: hand the game
/// to `choose_play` and get the play back.
#[scaling::bench(group = "play decision", baseline)]
fn choose_plays(situations: &Vec<String>) -> Vec<Option<String>> {
    situations
        .iter()
        .map(|situation| crate::choose_play(situation))
        .collect()
}

/// Plays any legal card, for a sense of how much the double-dummy loss of the
/// robot is worth.
#[scaling::bench(group = "play decision")]
fn random_plays(situations: &Vec<String>) -> Vec<Option<String>> {
    let mut rng = rand::thread_rng();
    situations
        .iter()
        .map(|situation| {
            let game: GameState<()> = serde_json::from_str(situation).ok()?;
            let legal: Vec<Card> = game.could_be_played().collect();
            let card = legal[rng.gen_range(0..legal.len())];
            serde_json::to_string(&Action::Play(game.hand_playing()?, card)).ok()
        })
        .collect()
}

/// The tricks that the side of the seat that is choosing would win with the
/// best play of the legal card `chosen`, less the tricks that it wins with
/// the best card, or `None` where that is not worked out.
fn loss(situation: &str, chosen: &Option<String>) -> Option<f64> {
    let game: GameState<()> = serde_json::from_str(situation).ok()?;
    let tricks_left = game.hands.iter().map(|hand| hand.len()).max()?;
    if tricks_left > DOUBLE_DUMMY_TRICKS {
        return None;
    }
    let chosen = match serde_json::from_str(chosen.as_ref()?).ok()? {
        Action::Play(_, card) => card,
        _ => return None,
    };
    let legal = game.could_be_played();
    if !legal.contains(chosen) {
        // Nothing is as bad as an illegal play.
        return Some(tricks_left as f64);
    }

    // The deal as the solver has it: the hands in the order of the play, with
    // each card already played to this trick back in its hand.
    let lead = game.lead?;
    let in_trick: &[Card] = if game.played.len() == 4 {
        &[]
    } else {
        &game.played
    };
    let mut hands = [Cards::EMPTY; 4];
    for seat in 0..4 {
        hands[(seat + 4 - lead as usize) % 4] = game.hands[Seat::from(seat)];
    }
    for (i, &card) in in_trick.iter().enumerate() {
        hands[i] += Cards::singleton(card);
    }
    let starting = Starting {
        hands,
        unknown: Cards::EMPTY,
    };
    let trump = match game.highest_contract_bid() {
        Some(Bid::Suit(_, trump)) => Some(trump),
        _ => None,
    };
    let mut solver = Naive::new(trump);
    let mut plays = in_trick.to_vec();
    let mut value = |card: Card| {
        plays.push(card);
        let value = trick_value(&mut solver, starting, trump, &mut plays);
        plays.pop();
        value
    };
    let values: Vec<(Card, f64)> = legal.map(|card| (card, value(card))).collect();
    let mine = values.iter().find(|&&(card, _)| card == chosen)?.1;
    // The score is for the side of whoever led the trick.
    let it_is_the_leaders_side = in_trick.len() & 1 == 0;
    let best = values
        .iter()
        .map(|&(_, value)| value)
        .fold(f64::NAN, |best, value| {
            if it_is_the_leaders_side {
                best.max(value)
            } else {
                best.min(value)
            }
        });
    Some(if it_is_the_leaders_side {
        best - mine
    } else {
        mine - best
    })
}

/// How many of the tricks left the side of whoever leads this trick wins,
/// when everything is known, after `plays` have been made to the trick.
fn trick_value(
    solver: &mut Naive,
    starting: Starting,
    trump: Option<Suit>,
    plays: &mut Vec<Card>,
) -> f64 {
    if plays.len() == 4 {
        let trick = starting.after([plays[0], plays[1], plays[2], plays[3]], trump);
        return (solver.score(trick.starting()) + trick).mean();
    }
    let seat = plays.len();
    let legal = if seat == 0 {
        starting.hands[0]
    } else {
        starting.hands[seat].following_suit(plays[0].suit())
    };
    let mut best: Option<f64> = None;
    for card in legal {
        plays.push(card);
        let value = trick_value(solver, starting, trump, plays);
        plays.pop();
        best = Some(match best {
            // The leader and the partner of the leader want the most tricks.
            Some(best) if seat & 1 == 0 => best.max(value),
            Some(best) => best.min(value),
            None => value,
        });
    }
    best.expect("a hand has to have a card to play")
}

/// How well the robot chose in a stage, as the mean tricks lost to the best
/// play and the share of the choices that lost any.
#[scaling::metrics(group = "play decision")]
fn play_quality(situations: &Vec<String>, chosen: Vec<Option<String>>) -> Metrics {
    let losses: Vec<f64> = situations
        .iter()
        .zip(&chosen)
        .filter_map(|(situation, chosen)| loss(situation, chosen))
        .collect();
    if losses.is_empty() {
        return Metrics::new();
    }
    let lost = losses.iter().filter(|&&loss| loss > 1e-9).count();
    Metrics::new()
        .ratio(
            "tricks lost",
            losses.iter().sum::<f64>() / losses.len() as f64,
        )
        .percent(
            "choices that lost",
            100.0 * lost as f64 / losses.len() as f64,
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The play that `card` is, as the robot would send it for this game.
    fn play_of(game: &GameState<()>, card: Card) -> Option<String> {
        let seat = game.hand_playing().unwrap();
        serde_json::to_string(&Action::Play(seat, card)).ok()
    }

    #[test]
    fn situations_are_the_same_every_time_and_have_a_choice() {
        for tricks_left in [13, 9, 5, 2] {
            let situations = play_situations(tricks_left);
            assert_eq!(situations_per_stage(), situations.len());
            assert_eq!(situations, play_situations(tricks_left));
            for situation in &situations {
                let game: GameState<()> = serde_json::from_str(situation).unwrap();
                assert_eq!(
                    tricks_left,
                    game.hands.iter().map(|hand| hand.len()).max().unwrap()
                );
                assert!(game.turn().is_some());
                assert!(game.could_be_played().len() > 1);
            }
        }
    }

    #[test]
    fn the_loss_of_the_best_card_is_nothing_and_no_card_has_less() {
        for tricks_left in [2, 3, 4] {
            for situation in play_situations(tricks_left) {
                let game: GameState<()> = serde_json::from_str(&situation).unwrap();
                let losses: Vec<f64> = game
                    .could_be_played()
                    .map(|card| loss(&situation, &play_of(&game, card)).unwrap())
                    .collect();
                assert!(losses.iter().all(|&loss| loss > -1e-9), "{losses:?}");
                assert!(losses.iter().any(|&loss| loss.abs() < 1e-9), "{losses:?}");
            }
        }
    }

    #[test]
    fn a_play_that_is_not_legal_loses_as_much_as_can_be() {
        let situation = &play_situations(3)[0];
        let game: GameState<()> = serde_json::from_str(situation).unwrap();
        let not_legal = (Cards::ALL - game.could_be_played()).next().unwrap();
        assert_eq!(Some(3.0), loss(situation, &play_of(&game, not_legal)),);
    }

    #[test]
    fn the_robot_plays_legal_cards() {
        for tricks_left in [2, 3] {
            let situations = play_situations(tricks_left);
            let chosen = choose_plays(&situations);
            for (situation, chosen) in situations.iter().zip(&chosen) {
                let loss = loss(situation, chosen).unwrap();
                assert!((0.0..tricks_left as f64).contains(&loss), "{loss}");
            }
        }
    }
}
