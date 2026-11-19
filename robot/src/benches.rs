//! Benchmarks of how the robot chooses a card to play, registered with
//! `scaling` when the `benchmarks` feature is on.
//!
//! A benchmark here is a stage of the play, which is how many tricks are left,
//! with some situations at that stage.  A situation is a deal that the robot
//! has bid to a contract with one of its bidding systems, and that has been
//! played part of the way through by random legal plays, and has someone to
//! choose a card.  It is kept as the JSON that the browser hands to
//! [`choose_play`](crate::choose_play), so that two versions of this crate are
//! handed the same situations and the same function.  The same situations are
//! made every time, so that what is measured is the robot and not the deals.
//! Sixteen of them in a row cover the four roles that a player can have (the
//! declarer playing their own hand, the declarer playing the dummy's, and
//! each of the defenders) with each of the four places in a trick, to play
//! first, second, third and last.
//!
//! Each benchmark reports how long a player takes to choose a card in every
//! situation of a stage, and how well it chose.  A card is valued over an
//! ensemble of deals of the two hands that the player cannot see, which fit
//! what is known of them: how many cards each has, the suits each has shown out
//! of, which cards have been played, and what the auction showed of each hand.
//! With `PLAY_BENCH_REFERENCE=replayed` it is narrowed to the deals that the
//! robot would have bid the auction with, or that come closest.
//! Its value in a deal is the tricks that it wins with the best play of
//! everyone when all the hands are known, and its value is the mean of that
//! over the ensemble.  The loss of a choice is the value of the best card,
//! less the value of the chosen card, in tricks.
//!
//! This is a yardstick and not a measure of how well anyone plays.  Best play
//! with every hand known is not how anyone plays, since a defender who cannot
//! see the declarer's hand should not play as though they could, and the
//! ensemble has every deal that fits the auction as likely as any other.  A
//! player who chooses by the same ensemble will do well by it.  So what is
//! meaningful is the difference between the players, and between the robot
//! and the random play that the group also has for scale.  The ensemble is the
//! same for every player and every version, and is not the one that
//! [`EnsemblePlay`](crate::EnsemblePlay) uses.
//!
//! Working out the best play gets too slow with more than
//! [`DOUBLE_DUMMY_TRICKS`] tricks left, and the earlier stages report no loss.
//! The robot samples deals for the cards that it cannot see, so its choices
//! vary from run to run, and so does the loss, which is a mean over the
//! situations of a stage.

use crate::ai::ensemble::{card_values, plays_for_the_leaders, starting_with, trump_of, Hidden};
use crate::ai::opening_lead::candidates::{
    linear_lead, opening_leads, pruned_lead, tuple_lead, TUPLES,
};
use crate::ai::opening_lead::{likely_opening_leads, Auction};
use crate::situations::Generator;
use crate::{Action, EnsemblePlay, GameState, PlayAI};
use bridge_deck::{Card, Cards};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use scaling::Metrics;

/// How many situations make up each stage by default, which is few enough for
/// a stage to be timed in a reasonable time.
const SITUATIONS: usize = 16;

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

/// Situations with `tricks_left` tricks left, which are the same every time.
pub fn play_situations(tricks_left: usize) -> Vec<String> {
    let mut generator = Generator::new();
    let mut rng = StdRng::seed_from_u64(tricks_left as u64);
    let mut found = Vec::new();
    let wanted = situations_per_stage();
    while found.len() < wanted {
        found.extend(generator.situation(&mut rng, found.len(), tricks_left));
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

/// The robot with the cards played by the play model that self-play learned
/// (`choose_play_by_model`), which chooses without searching.  Without the
/// model's weights it plays as the robot does.
#[scaling::bench(group = "play decision")]
fn model_plays(situations: &Vec<String>) -> Vec<Option<String>> {
    situations
        .iter()
        .map(|situation| crate::ai::choose_play_by_model(situation))
        .collect()
}

/// The play of [`EnsemblePlay`], which chooses by a smaller ensemble, in the
/// same way that the loss is measured.  It keeps what it has learnt from one
/// situation to the next, and so does not read the bidding system again each
/// time, as `choose_play` does.
#[scaling::bench(group = "play decision")]
fn ensemble_plays(situations: &Vec<String>) -> Vec<Option<String>> {
    let mut player = EnsemblePlay::new();
    situations
        .iter()
        .map(|situation| {
            let game: GameState<()> = serde_json::from_str(situation).ok()?;
            let card = player.play(&game);
            serde_json::to_string(&Action::Play(game.hand_playing()?, card)).ok()
        })
        .collect()
}

/// The play of [`EnsemblePlay`] when it [replays the
/// auction](EnsemblePlay::replaying_the_auction), and so chooses by the deals
/// that the robot would have bid the auction with.
#[scaling::bench(group = "play decision")]
fn replayed_ensemble_plays(situations: &Vec<String>) -> Vec<Option<String>> {
    let mut player = EnsemblePlay::new().replaying_the_auction();
    situations
        .iter()
        .map(|situation| {
            let game: GameState<()> = serde_json::from_str(situation).ok()?;
            let card = player.play(&game);
            serde_json::to_string(&Action::Play(game.hand_playing()?, card)).ok()
        })
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

/// How many deals of the hands that the player cannot see make up the
/// ensemble that a play is valued over.
const REFERENCE_DEALS: usize = 48;

/// Whether the ensemble that a play is valued over is narrowed to the deals
/// that come closest to being bid as the auction was, which it is when the
/// environment variable `PLAY_BENCH_REFERENCE` is `replayed`.  It is then
/// smaller, and often much smaller, than [`REFERENCE_DEALS`].
fn reference_replays() -> bool {
    std::env::var("PLAY_BENCH_REFERENCE").is_ok_and(|reference| reference == "replayed")
}

/// A number made from `text`, which is the same on every platform.
fn seed_from(text: &str) -> u64 {
    text.bytes().fold(0xcbf2_9ce4_8422_2325, |hash, byte| {
        (hash ^ byte as u64).wrapping_mul(0x0100_0000_01b3)
    })
}

/// How many tricks the best play would win, over an ensemble of the hands that
/// the player cannot see that fit the auction and the play so far, more than
/// the play that was chosen would, or `None` where that is not worked out.
///
/// The value of a play is the number of tricks that the side of whoever leads
/// the trick wins with best play by everyone, averaged over the ensemble.  The
/// ensemble is the same for every play that is chosen in a situation, and for
/// every version of the robot.
fn loss(situation: &str, chosen: &Option<String>) -> Option<f64> {
    loss_by(situation, chosen, reference_values)
}

/// How many tricks the best play would win in the real deal, which the
/// situation has, with best play by everyone after it, more than the play that
/// was chosen would, or `None` where that is not worked out.  This does not
/// favour a player who chooses by an ensemble like the reference one, as
/// [`loss`] does, but it is noisier: a play that is best in most deals that
/// fit can lose in the one that was dealt.
fn real_loss(situation: &str, chosen: &Option<String>) -> Option<f64> {
    loss_by(situation, chosen, real_values)
}

/// The loss of the play that was chosen, by `values`, which values every legal
/// card of a situation.
fn loss_by(
    situation: &str,
    chosen: &Option<String>,
    values: fn(&str, &GameState<()>) -> Option<Values>,
) -> Option<f64> {
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
    let means = values(situation, &game)?;
    let mine = means.iter().find(|&&(card, _)| card == chosen)?.1;
    let best = if plays_for_the_leaders(&game) {
        means.iter().map(|&(_, mean)| mean).fold(f64::MIN, f64::max)
    } else {
        means.iter().map(|&(_, mean)| mean).fold(f64::MAX, f64::min)
    };
    Some((best - mine).abs())
}

/// The value of each legal card of a situation.
type Values = Vec<(Card, f64)>;

thread_local! {
    /// The values that `reference_values` and `real_values` have worked out,
    /// since every player of a situation is valued by the same ones.
    static REMEMBERED: std::cell::RefCell<std::collections::HashMap<String, Option<Values>>>
        = Default::default();
}

/// The value of each legal card in the situation: the tricks that the side of
/// whoever leads the trick wins by playing it, with best play by everyone after
/// that, averaged over the deals of the ensemble.
fn reference_values(situation: &str, game: &GameState<()>) -> Option<Values> {
    remembered("reference", situation, || {
        work_out_reference_values(situation, game)
    })
}

/// The value of each legal card in the real deal: the tricks that the side of
/// whoever leads the trick wins by playing it, with best play by everyone after
/// that.
fn real_values(situation: &str, game: &GameState<()>) -> Option<Values> {
    remembered("real", situation, || {
        let convention = game.system.convention();
        let hidden = Hidden::new(game, &convention)?;
        let real = hidden.seats().map(|seat| game.hands[seat]);
        let (starting, in_trick) = starting_with(game, &hidden, real)?;
        Some(card_values(
            starting,
            trump_of(game),
            &in_trick,
            game.could_be_played(),
        ))
    })
}

/// The values of a situation that were worked out as `kind` before, or else
/// those that `work_out` gives, which are remembered.
fn remembered(
    kind: &str,
    situation: &str,
    work_out: impl FnOnce() -> Option<Values>,
) -> Option<Values> {
    let key = format!("{kind} {situation}");
    if let Some(known) = REMEMBERED.with(|r| r.borrow().get(&key).cloned()) {
        return known;
    }
    let values = work_out();
    REMEMBERED.with(|r| r.borrow_mut().insert(key, values.clone()));
    values
}

fn work_out_reference_values(situation: &str, game: &GameState<()>) -> Option<Values> {
    let convention = game.system.convention();
    let hidden = Hidden::new(game, &convention)?;
    let mut rng = StdRng::seed_from_u64(seed_from(situation));
    let replay = reference_replays().then_some(&convention);
    let deals = hidden.ensemble(&mut rng, REFERENCE_DEALS, replay);
    if deals.is_empty() {
        return None;
    }
    let trump = trump_of(game);
    let legal = game.could_be_played();
    let mut totals: Vec<(Card, f64)> = legal.map(|card| (card, 0.0)).collect();
    for &deal in &deals {
        let (starting, in_trick) = starting_with(game, &hidden, deal)?;
        for (total, (_, value)) in totals
            .iter_mut()
            .zip(card_values(starting, trump, &in_trick, legal))
        {
            total.1 += value;
        }
    }
    Some(
        totals
            .into_iter()
            .map(|(card, total)| (card, total / deals.len() as f64))
            .collect(),
    )
}

/// How well the robot chose in a stage, as the mean tricks lost to the best
/// play over the reference ensemble and in the real deal, and the share of the
/// choices that lost any over the ensemble.
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
    let real: Vec<f64> = situations
        .iter()
        .zip(&chosen)
        .filter_map(|(situation, chosen)| real_loss(situation, chosen))
        .collect();
    Metrics::new()
        .ratio(
            "tricks lost in the real deal",
            real.iter().sum::<f64>() / real.len() as f64,
        )
        .ratio(
            "tricks lost",
            losses.iter().sum::<f64>() / losses.len() as f64,
        )
        .percent(
            "choices that lost",
            100.0 * lost as f64 / losses.len() as f64,
        )
}

/// The opening leads of the "opening lead" group: the auction as the leader
/// sees it and the leader's hand, for 64 opening leads that the robot bid
/// to, the same every time.  Only this crate's working tree registers the
/// group, so the input can be its own types.
#[scaling::input(group = "opening lead", name = "64 leads", reusable)]
fn opening_lead_hands() -> Vec<(Auction, Cards)> {
    opening_leads(1, 64)
        .iter()
        .map(|game| (Auction::new(game).unwrap(), game.hands[game.turn().unwrap()]))
        .collect()
}

/// The lead that the 35 fitted features score highest, which is the lead
/// the robot's order of opening leads puts first.
#[scaling::bench(group = "opening lead", baseline)]
fn fitted_features(leads: &Vec<(Auction, Cards)>) -> Vec<Card> {
    leads.iter().map(|(auction, hand)| linear_lead(auction, *hand)).collect()
}

/// The lead that an n-tuple network scores highest, with weights that are
/// made up, since they do not change how long it takes.
#[scaling::bench(group = "opening lead")]
fn n_tuple_network(leads: &Vec<(Auction, Cards)>) -> Vec<Card> {
    static WEIGHTS: std::sync::OnceLock<Vec<f64>> = std::sync::OnceLock::new();
    let weights = WEIGHTS.get_or_init(|| {
        let size: usize = TUPLES.iter().map(|t| t.0).sum();
        (0..size).map(|i| (i * 2_654_435_761 % 1000) as f64 / 1000.0 - 0.5).collect()
    });
    leads
        .iter()
        .map(|(auction, hand)| tuple_lead(weights, auction, *hand))
        .collect()
}

/// The lead that the pruned network for suit contracts scores highest,
/// written out as its closed-form sum.
#[scaling::bench(group = "opening lead")]
fn pruned_network(leads: &Vec<(Auction, Cards)>) -> Vec<Card> {
    leads.iter().map(|(auction, hand)| pruned_lead(auction, *hand)).collect()
}

/// The first of the leads that `likely_opening_leads` sorts, which is what
/// the robot calls.
#[scaling::bench(group = "opening lead")]
fn sorted_fitted_features(leads: &Vec<(Auction, Cards)>) -> Vec<Card> {
    leads
        .iter()
        .map(|(auction, hand)| likely_opening_leads(auction, *hand)[0])
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use bridge_deck::Cards;

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
    fn sixteen_situations_cover_every_role_and_place_in_the_trick() {
        for tricks_left in [11, 7, 3] {
            let situations = play_situations(tricks_left);
            assert_eq!(16, situations_per_stage());
            let mut seen = std::collections::BTreeSet::new();
            for situation in &situations {
                let game: GameState<()> = serde_json::from_str(situation).unwrap();
                seen.extend(crate::situations::role_and_place(&game));
            }
            assert_eq!(16, seen.len(), "{seen:?} with {tricks_left} tricks left");
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

    /// The mean loss of each player with 3 to 7 tricks left, for comparing
    /// their quality with more situations than a timed run wants:
    /// `PLAY_BENCH_SITUATIONS=48 cargo test -p robot --release --features
    /// benchmarks compare_the_players -- --ignored --nocapture`, with
    /// `PLAY_BENCH_REFERENCE=replayed` to value the plays by the replayed
    /// ensemble.
    #[test]
    #[ignore]
    fn compare_the_players() {
        type Player = fn(&Vec<String>) -> Vec<Option<String>>;
        let players: [(&str, Player); 4] = [
            ("robot", choose_plays),
            ("ensemble", ensemble_plays),
            ("replayed ensemble", replayed_ensemble_plays),
            ("random", random_plays),
        ];
        println!("reference replayed: {}", reference_replays());
        for tricks_left in (3..=7).rev() {
            let situations = play_situations(tricks_left);
            let losses: Vec<String> = players
                .iter()
                .map(|(name, player)| {
                    let chosen = player(&situations);
                    let mean = |loss: fn(&str, &Option<String>) -> Option<f64>| {
                        let losses: Vec<f64> = situations
                            .iter()
                            .zip(&chosen)
                            .filter_map(|(situation, chosen)| loss(situation, chosen))
                            .collect();
                        losses.iter().sum::<f64>() / losses.len() as f64
                    };
                    format!("{name} {:.3} (real {:.3})", mean(loss), mean(real_loss))
                })
                .collect();
            println!("{tricks_left} tricks left: {}", losses.join(", "));
        }
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
