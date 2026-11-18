//! A player that chooses a card without searching: an n-tuple network over
//! what the player can see (their hand, the other hand in sight, which cards
//! are still hidden, the trick so far and the auction), whose entries are
//! fitted to the double-dummy value of each card averaged over deals of the
//! hidden hands that fit what is known, as `EnsemblePlay` values them.  It is
//! for the stages of the play that are too slow to search, and for choosing
//! which few cards a search should value.  Nothing here says which card is
//! good, only which facts each table sees; the opening lead has its own
//! tables (`opening_lead`), since the dummy is not yet in sight then.

use crate::{Bid, GameState, Seat};
use bridge_deck::{Card, Cards, Suit};

/// The tables, each with its size and the facts that index it.  The role is
/// 0 for the declarer playing their own hand, 1 for the declarer playing the
/// dummy's, and 2 and 3 for the defenders on the declarer's left and right;
/// the place is how many cards have been played to the trick; how a card is
/// played is whether it leads, follows suit, ruffs or is discarded.
pub(crate) const TUPLES: [(usize, &str); 7] = [
    (
        4 * 4 * 4 * 2 * 2,
        "the role, the place, how the card is played, whether partner is \
         winning the trick, and whether the card beats the trick so far",
    ),
    (
        4 * 125,
        "the place, and whether each of the three cards just above is the \
         player's, in the other hand in sight, or hidden",
    ),
    (4 * 125, "the place, and the same of the three cards just below"),
    (
        4 * 4 * 6 * 2,
        "the place, how the card is played, how many cards of its suit are left \
         above it, and whether it is the player's lowest of the suit",
    ),
    (
        4 * 4 * 4 * 2,
        "the place, the player's length in the suit, the length of the other \
         hand in sight, and whether the suit is trumps",
    ),
    (4 * 4 * 6, "the place, how the card is played, and how many tricks are left"),
    (
        4 * 4 * 4,
        "the role, how the card is played, and which sides bid its suit",
    ),
];

/// How many entries the tables have, end to end.
pub(crate) const ENTRIES: usize = {
    let mut entries = 0;
    let mut i = 0;
    while i < TUPLES.len() {
        entries += TUPLES[i].0;
        i += 1;
    }
    entries
};

/// What the player to choose a card can see, which every card they could
/// play shares.
pub(crate) struct View {
    role: usize,
    place: usize,
    trump: Option<Suit>,
    /// The hand that the card is played from.
    mine: Cards,
    /// The other hand in sight: the dummy, or for the dummy the declarer's.
    other: Cards,
    /// The cards of the two hands that cannot be seen.
    hidden: Cards,
    /// Every card that has not been played.
    left: Cards,
    lead: Option<Suit>,
    winning: Option<Card>,
    partner_winning: bool,
    /// Which suits each side has bid: the player's side and the other.
    bid: [[bool; 4]; 2],
}

impl View {
    /// What the player to play a card in `game` can see, or `None` when
    /// nobody is to play a card, or it is the opening lead.
    pub(crate) fn new<C>(game: &GameState<C>) -> Option<View> {
        let lead_seat = game.lead?;
        let dummy = game.dummy()?;
        let declarer = dummy + 2;
        let in_trick: &[Card] = if game.played.len() == 4 {
            &[]
        } else {
            &game.played
        };
        let place = in_trick.len();
        let seat = lead_seat + place;
        let left = (0..4).fold(Cards::EMPTY, |cards, s| cards + game.hands[Seat::from(s)]);
        if left.len() == 52 {
            return None;
        }
        let role = if seat == declarer {
            0
        } else if seat == dummy {
            1
        } else if seat == declarer + 1 {
            2
        } else {
            3
        };
        let mine = game.hands[seat];
        let other = if seat == dummy {
            game.hands[declarer]
        } else {
            game.hands[dummy]
        };
        let trump = super::ensemble::trump_of(game);
        let mut winner = 0;
        for (i, &card) in in_trick.iter().enumerate().skip(1) {
            if beats(trump, card, in_trick[winner]) {
                winner = i;
            }
        }
        let mut bid = [[false; 4]; 2];
        for (i, &call) in game.bids.iter().enumerate() {
            if let Bid::Suit(_, suit) = call {
                let bidder = game.dealer + i;
                let theirs = (bidder as usize + 4 - seat as usize) % 2;
                bid[theirs][suit as usize] = true;
            }
        }
        Some(View {
            role,
            place,
            trump,
            mine,
            other,
            hidden: left - mine - other,
            left,
            lead: in_trick.first().map(|card| card.suit()),
            winning: in_trick.get(winner).copied(),
            partner_winning: place > 0 && winner % 2 == place % 2,
            bid,
        })
    }

    /// The entries of the tables of [`TUPLES`], laid end to end, that
    /// playing `card` looks up, one in each table.
    pub(crate) fn entries(&self, card: Card) -> [usize; TUPLES.len()] {
        let suit = card.suit();
        let (role, place) = (self.role, self.place);
        let how = match self.lead {
            None => 0,
            Some(lead) if lead == suit => 1,
            Some(_) if self.trump == Some(suit) => 2,
            Some(_) => 3,
        };
        let beats = self
            .winning
            .is_some_and(|winning| beats(self.trump, card, winning));
        // Renumbered among the cards left, the cards just above and below
        // are the next ones still in play.
        let renumbered = |cards: Cards| cards.renumbered_among(self.left);
        let in_sight = [
            renumbered(self.mine),
            renumbered(self.other),
            renumbered(self.hidden),
            Cards::EMPTY,
        ];
        let card_now = renumbered(Cards::singleton(card)).max().unwrap();
        let mine = self.mine.in_suit(suit);
        let above = (self.left.in_suit(suit) & Cards::higher_than(card))
            .len()
            .min(5);
        let lowest = mine.min() == Some(card);
        let length = mine.len().min(4) - 1;
        let other_length = self.other.in_suit(suit).len().min(3);
        let trumps = self.trump == Some(suit);
        let tricks = match self.mine.len() {
            0..=1 => 0,
            2 => 1,
            3 => 2,
            4..=5 => 3,
            6..=8 => 4,
            _ => 5,
        };
        let bid_by = self.bid[0][suit as usize] as usize | (self.bid[1][suit as usize] as usize) << 1;
        let indices = [
            (((role * 4 + place) * 4 + how) * 2 + self.partner_winning as usize) * 2 + beats as usize,
            place * 125 + Cards::holders_above(&in_sight, card_now, 3),
            place * 125 + Cards::holders_below(&in_sight, card_now, 3),
            ((place * 4 + how) * 6 + above) * 2 + lowest as usize,
            ((place * 4 + length) * 4 + other_length) * 2 + trumps as usize,
            (place * 4 + how) * 6 + tricks,
            (role * 4 + how) * 4 + bid_by,
        ];
        let mut entries = [0; TUPLES.len()];
        let mut offset = 0;
        for (t, index) in indices.into_iter().enumerate() {
            debug_assert!(index < TUPLES[t].0, "{}", TUPLES[t].1);
            entries[t] = offset + index;
            offset += TUPLES[t].0;
        }
        entries
    }
}

/// Whether `card`, played after `winning`, beats it.
fn beats(trump: Option<Suit>, card: Card, winning: Card) -> bool {
    if card.suit() == winning.suit() {
        card.rank() > winning.rank()
    } else {
        Some(card.suit()) == trump
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai::ensemble::{card_values, plays_for_the_leaders, starting_with, trump_of, Hidden};
    use crate::ai::fitting;
    use crate::situations::Generator;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    fn var(name: &str, default: &str) -> String {
        std::env::var(name).unwrap_or_else(|_| default.into())
    }

    fn number(name: &str, default: usize) -> usize {
        var(name, &default.to_string()).parse().unwrap()
    }

    fn pair(card: Card) -> [u8; 2] {
        [card.suit() as u8, card.rank()]
    }

    #[test]
    fn every_card_that_could_be_played_has_its_entries() {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(1);
        for tricks_left in 2..=12 {
            for index in 0..16 {
                let Some(json) = generator.situation(&mut rng, index, tricks_left) else {
                    continue;
                };
                let game: GameState<()> = serde_json::from_str(&json).unwrap();
                let view = View::new(&game).unwrap();
                for card in game.could_be_played() {
                    for (t, entry) in view.entries(card).into_iter().enumerate() {
                        let start: usize = TUPLES[..t].iter().map(|t| t.0).sum();
                        assert!((start..start + TUPLES[t].0).contains(&entry));
                    }
                }
            }
        }
    }

    /// Writes situations of the play, with the double-dummy value of each
    /// card that could be played on each of an ensemble of deals of the
    /// hidden hands, one JSON object per line, to the file `PLAY_DATA`: the
    /// stage (tricks left), the game as JSON, whether the player plays for
    /// the side that led the trick, and for each card the tricks that side
    /// takes after it in each deal.  The stages are `PLAY_STAGES` (7, 6, 5,
    /// 4, 3, 2), each with `PLAY_SITUATIONS` (200) situations from the seed
    /// `PLAY_SEED` (1), and the ensembles have `PLAY_DEALS` (6) deals, which
    /// replay the auction as `EnsemblePlay::replaying_the_auction` does.  It
    /// uses every core.
    #[test]
    #[ignore]
    fn collect_play_data() {
        let path = std::env::var("PLAY_DATA").expect("PLAY_DATA");
        let stages: Vec<usize> = var("PLAY_STAGES", "7,6,5,4,3,2")
            .split(',')
            .map(|s| s.trim().parse().unwrap())
            .collect();
        let count = number("PLAY_SITUATIONS", 200);
        let deals = number("PLAY_DEALS", 6);
        let seed = number("PLAY_SEED", 1) as u64;
        let mut generator = Generator::new();
        let mut situations = Vec::new();
        for &stage in &stages {
            let mut rng = StdRng::seed_from_u64(seed * 1000 + stage as u64);
            let mut index = 0;
            let start = situations.len();
            while situations.len() < start + count {
                if let Some(json) = generator.situation(&mut rng, index, stage) {
                    situations.push((stage, json));
                }
                index += 1;
            }
        }
        let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
        let next = std::sync::atomic::AtomicUsize::new(0);
        let lines = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    let mut conventions = std::collections::HashMap::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some((stage, json)) = situations.get(i) else {
                            break;
                        };
                        let game: GameState<()> = serde_json::from_str(json).unwrap();
                        let convention = conventions
                            .entry(format!("{:?}", game.system))
                            .or_insert_with(|| game.system.convention());
                        let Some(hidden) = Hidden::new(&game, convention) else {
                            continue;
                        };
                        let mut rng = StdRng::seed_from_u64(seed * 1_000_000 + i as u64);
                        let legal = game.could_be_played();
                        let trump = trump_of(&game);
                        let mut values: Vec<Vec<f64>> = legal.map(|_| Vec::new()).collect();
                        for deal in hidden.ensemble(&mut rng, deals, Some(convention)) {
                            let Some((starting, in_trick)) = starting_with(&game, &hidden, deal) else {
                                continue;
                            };
                            for (all, (_, value)) in values.iter_mut().zip(card_values(starting, trump, &in_trick, legal)) {
                                all.push(value);
                            }
                        }
                        if values.iter().any(|v| v.is_empty()) {
                            continue;
                        }
                        let line = serde_json::json!({
                            "stage": stage,
                            "situation": json,
                            "leaders": plays_for_the_leaders(&game),
                            "values": legal.map(pair).zip(values).collect::<Vec<_>>(),
                        });
                        lines.lock().unwrap().push((i, line.to_string()));
                    }
                });
            }
        });
        let mut lines = lines.into_inner().unwrap();
        lines.sort();
        let text: String = lines.into_iter().map(|(_, line)| line + "\n").collect();
        std::fs::write(path, text).unwrap();
    }

    /// A situation from the data that `collect_play_data` writes, as the
    /// choices of `fitting`: each card's entries and its mean value to the
    /// player, the tricks of their side, over the ensemble.
    struct Situation {
        stage: usize,
        choice: Vec<(Vec<usize>, f64)>,
    }

    /// The situations in the files `paths`, comma-separated.
    fn read_situations(paths: &str) -> Vec<Situation> {
        let mut situations = Vec::new();
        for path in paths.split(',') {
            for line in std::fs::read_to_string(path.trim()).unwrap().lines() {
                let json: serde_json::Value = serde_json::from_str(line).unwrap();
                let game: GameState<()> =
                    serde_json::from_str(json["situation"].as_str().unwrap()).unwrap();
                let view = View::new(&game).unwrap();
                let leaders = json["leaders"].as_bool().unwrap();
                let choice = json["values"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| {
                        let card = Card::new(
                            Suit::ALL[v[0][0].as_u64().unwrap() as usize],
                            v[0][1].as_u64().unwrap() as u8,
                        );
                        let deals = v[1].as_array().unwrap();
                        let mean = deals.iter().map(|d| d.as_f64().unwrap()).sum::<f64>()
                            / deals.len() as f64;
                        // The tricks of the player's side, up to a number that
                        // is the same for every card.
                        let value = if leaders { mean } else { -mean };
                        (view.entries(card).to_vec(), value)
                    })
                    .collect();
                situations.push(Situation {
                    stage: json["stage"].as_u64().unwrap() as usize,
                    choice,
                });
            }
        }
        situations
    }

    /// Fits the play model to the data in the files `PLAY_DATA`, picking the
    /// penalty by five-fold cross-validation, and prints, for each stage of
    /// the data in `PLAY_TEST`, the tricks that its choice loses to the best
    /// card by the ensemble's values, beside those that a card at random
    /// loses.  Writes the weights, one a line, to the file `PLAY_WEIGHTS` if
    /// it is set.  `PLAY_STEPS` (500) is the number of steps of each fit.
    #[test]
    #[ignore]
    fn fit_play_model() {
        let train = read_situations(&std::env::var("PLAY_DATA").expect("PLAY_DATA"));
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"));
        let steps = number("PLAY_STEPS", 500);
        let choices: fitting::Choices = train.into_iter().map(|s| s.choice).collect();
        let (penalty, held_out) =
            fitting::cross_validate(&choices, ENTRIES, &[0.0, 0.0001, 0.001, 0.01], steps);
        for (penalty, value) in held_out {
            println!("penalty {penalty}: {value:.4} held out");
        }
        let weights = fitting::fit(&choices.iter().collect::<Vec<_>>(), ENTRIES, penalty, steps);
        let mut stages: Vec<usize> = test.iter().map(|s| s.stage).collect();
        stages.sort_unstable();
        stages.dedup();
        for stage in stages.into_iter().rev() {
            let at: Vec<&Situation> = test.iter().filter(|s| s.stage == stage).collect();
            let loss = |value: &dyn Fn(&Situation) -> f64| {
                at.iter()
                    .map(|s| {
                        let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                        best - value(s)
                    })
                    .sum::<f64>()
                    / at.len() as f64
            };
            let model = loss(&|s| s.choice[fitting::choice(&weights, &s.choice)].1);
            let random = loss(&|s| s.choice.iter().map(|c| c.1).sum::<f64>() / s.choice.len() as f64);
            println!(
                "{stage} tricks left, {} situations: the model loses {model:.3} tricks a play, a card at random {random:.3}",
                at.len()
            );
        }
        if let Ok(path) = std::env::var("PLAY_WEIGHTS") {
            let text: String = weights.iter().map(|w| format!("{w}\n")).collect();
            std::fs::write(path, text).unwrap();
        }
    }
}
