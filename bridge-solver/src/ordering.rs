//! Which card a player is likely to play, guessed from the cards alone,
//! without looking ahead.
//!
//! [`likely_plays`] puts the cards that the next player to a trick may play in
//! the order in which they are likely to be best.  A search that tries them in
//! that order finds a good play early, and once it has found a play good
//! enough, it need not try the others.

use bridge_deck::{Card, Cards, Suit};

#[cfg(test)]
pub(crate) mod fitted;

/// The cards worth trying for the next player to `trick`, from `hands`, the
/// likeliest best first.  The `hands` are as they were before the trick, in
/// the order of play from its leader, so the next player holds
/// `hands[trick.len()]`, and every card is known.  Of cards that touch, such
/// as a king and queen once the ace is gone, only the lowest is given, since
/// any one of them takes the same tricks as the others.
///
/// The leader first cashes a winner that cannot be ruffed, and otherwise leads
/// low toward partner's winner or to partner's ruff.  The others win the
/// trick as cheaply as they can, with a card that the player after them cannot
/// beat if they have one, unless partner is already winning it, and otherwise
/// play low, or discard a low card that is not a winner.
pub fn likely_plays(
    trump: Option<Suit>,
    hands: &[Cards; 4],
    trick: &[Card],
) -> impl Iterator<Item = Card> {
    debug_assert!(trick.len() < 4, "a trick has four cards");
    let table = Table {
        trump,
        hands,
        trick,
        all: hands[0] + hands[1] + hands[2] + hands[3],
    };
    let tiers = if trick.is_empty() {
        table.leads()
    } else {
        table.follows()
    };
    // Within each tier, the cards come lowest first.
    IntoIterator::into_iter(tiers).flatten()
}

/// What the next player to a trick can see.
struct Table<'a> {
    trump: Option<Suit>,
    hands: &'a [Cards; 4],
    trick: &'a [Card],
    /// The cards that have not been played before this trick.
    all: Cards,
}

impl Table<'_> {
    /// The leads, in tiers from the likeliest best to the least.
    fn leads(&self) -> [Cards; 6] {
        let hand = self.hands[0];
        let plays = hand.lowest_of_sequences(self.all);
        let [mut winners, mut to_partner, mut to_ruff, mut low, mut others, mut ruffed] =
            [Cards::EMPTY; 6];
        for suit in Suit::ALL {
            let mine = plays.in_suit(suit);
            let Some(lowest) = mine.min() else {
                continue;
            };
            if self.can_ruff(1, suit) || self.can_ruff(3, suit) {
                ruffed += mine;
                continue;
            }
            let top = self.all.in_suit(suit).max();
            let mut rest = mine;
            if let Some(top) = top.filter(|&top| mine.contains(top)) {
                // A winner, which wins the trick.
                winners += Cards::singleton(top);
                rest -= Cards::singleton(top);
            }
            if rest.contains(lowest) {
                if top.is_some_and(|top| self.hands[2].contains(top)) {
                    to_partner += Cards::singleton(lowest);
                } else if self.can_ruff(2, suit) {
                    to_ruff += Cards::singleton(lowest);
                } else {
                    low += Cards::singleton(lowest);
                }
                rest -= Cards::singleton(lowest);
            }
            others += rest;
        }
        [winners, to_partner, to_ruff, low, others, ruffed]
    }

    /// The plays of a player who follows the leader, in tiers from the
    /// likeliest best to the least.
    fn follows(&self) -> [Cards; 6] {
        let played = self.trick.len();
        let lead = self.trick[0].suit();
        let hand = self.hands[played];
        let plays = hand.following_suit(lead).lowest_of_sequences(self.all);
        let winner = self.winner_so_far();
        let winning = self.trick[winner];
        // The cards that would win the trick so far.
        let mut beating = plays & Cards::higher_than(winning);
        if let Some(trump) = self.trump.filter(|&trump| trump != winning.suit()) {
            beating += plays.in_suit(trump);
        }
        let holding = beating
            .filter(|&card| self.holds(card))
            .fold(Cards::EMPTY, Cards::insert);
        // A discard is a card of neither the suit led nor trumps, and the
        // top card of a suit is the worst one to throw away.
        let discards = if plays.in_suit(lead).is_empty() {
            match self.trump {
                Some(trump) => plays - plays.in_suit(trump),
                None => plays,
            }
        } else {
            Cards::EMPTY
        };
        let tops = discards.filter(|&card| self.all.in_suit(card.suit()).max() == Some(card));
        let tops = tops.fold(Cards::EMPTY, Cards::insert);
        let discards = discards - tops;
        // The low cards of the suit led, or, for a player who has none, the
        // trumps too low to win.
        let low = plays - beating - discards - tops;
        if winner & 1 == played & 1 {
            // Partner is winning.
            if self.holds(winning) {
                [discards, low, tops, beating, Cards::EMPTY, Cards::EMPTY]
            } else {
                [
                    holding,
                    discards,
                    low,
                    tops,
                    beating - holding,
                    Cards::EMPTY,
                ]
            }
        } else {
            [
                holding,
                beating - holding,
                discards,
                low,
                tops,
                Cards::EMPTY,
            ]
        }
    }

    /// Whether `card`, winning the trick, would still win it after the
    /// player after the next one has played.  The player after that is
    /// partner, who cannot take the trick from their own side.
    fn holds(&self, card: Card) -> bool {
        let played = self.trick.len();
        if played == 3 {
            return true;
        }
        let later = self.hands[(played + 1) % 4].following_suit(self.trick[0].suit());
        if !(later & Cards::higher_than(card)).is_empty() {
            return false;
        }
        match self.trump {
            Some(trump) if trump != card.suit() => later.in_suit(trump).is_empty(),
            _ => true,
        }
    }

    /// Whether the player `seat` from the leader can ruff a lead of `suit`.
    fn can_ruff(&self, seat: usize, suit: Suit) -> bool {
        match self.trump {
            Some(trump) if trump != suit => {
                self.hands[seat].in_suit(suit).is_empty()
                    && !self.hands[seat].in_suit(trump).is_empty()
            }
            _ => false,
        }
    }

    /// Which of the cards of the trick so far is winning it.
    fn winner_so_far(&self) -> usize {
        let mut winner = 0;
        for (i, &card) in self.trick.iter().enumerate().skip(1) {
            if self.beats(card, self.trick[winner]) {
                winner = i;
            }
        }
        winner
    }

    /// Whether `card`, played after `winning`, beats it.
    fn beats(&self, card: Card, winning: Card) -> bool {
        if card.suit() == winning.suit() {
            card.rank() > winning.rank()
        } else {
            Some(card.suit()) == self.trump
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DoubleDummy;
    use rand::rngs::SmallRng;
    use rand::{Rng, SeedableRng};

    fn hands(hands: [&str; 4]) -> [Cards; 4] {
        hands.map(|hand| hand.parse().unwrap())
    }

    fn cards(cards: &str) -> Vec<Card> {
        let cards: Cards = cards.parse().unwrap();
        cards.collect()
    }

    fn first(trump: Option<Suit>, hands: &[Cards; 4], trick: &[Card]) -> Card {
        likely_plays(trump, hands, trick).next().unwrap()
    }

    #[test]
    fn the_leader_cashes_a_winner_that_cannot_be_ruffed() {
        let deal = hands(["S A2 H 2", "S 3 H 34", "S 4 H 56", "S 5 H 78"]);
        assert_eq!(cards("S A")[0], first(None, &deal, &[]));
        // With hearts trumps and the next player out of spades, the ace of
        // spades would be ruffed.
        let deal = hands(["S A2 H 2", "D 3 H 34", "S 4 H 56", "S 5 H 78"]);
        assert_ne!(cards("S A")[0], first(Some(Suit::Hearts), &deal, &[]));
    }

    #[test]
    fn the_leader_leads_low_to_partners_winner() {
        let deal = hands(["S 32 H 2", "S 4 H 34", "S A H 56", "S 5 H 78"]);
        assert_eq!(cards("S 2")[0], first(None, &deal, &[]));
    }

    #[test]
    fn the_second_hand_wins_with_a_card_that_holds() {
        // The king beats the leader's ten and the queen that third hand
        // holds, and the jack does not.
        let deal = hands(["S T", "S KJ", "S Q", "S 2"]);
        assert_eq!(cards("S K")[0], first(None, &deal, &cards("S T")));
    }

    #[test]
    fn partners_winner_gets_a_low_card() {
        // Partner's ace wins, so third hand plays low and keeps the king.
        let deal = hands(["S A", "S 2", "S K3", "S 4"]);
        let trick = [cards("S A")[0], cards("S 2")[0]];
        assert_eq!(cards("S 3")[0], first(None, &deal, &trick));
    }

    #[test]
    fn the_last_hand_wins_as_cheaply_as_it_can() {
        let deal = hands(["S 9", "S 2", "S 3", "S AJT"]);
        let trick = cards("S 9 3 2");
        let trick = [trick[2], trick[0], trick[1]];
        assert_eq!(cards("S T")[0], first(None, &deal, &trick));
    }

    #[test]
    fn a_discard_is_a_low_card_rather_than_a_winner() {
        let deal = hands(["S 9", "S 2", "S 3", "H A D 2"]);
        let trick = [cards("S 9")[0], cards("S 2")[0], cards("S 3")[0]];
        assert_eq!(cards("D 2")[0], first(None, &deal, &trick));
    }

    fn deal(rng: &mut SmallRng, size: usize) -> [Cards; 4] {
        let mut deck = Cards::ALL;
        [0; 4].map(|_| deck.pick_rng(rng, size).unwrap())
    }

    /// Plays a deal through from the start, picking each card at random, and
    /// gives each position along the way: the hands before the trick and the
    /// cards played to it so far.
    fn positions(rng: &mut SmallRng, mut hands: [Cards; 4]) -> Vec<([Cards; 4], Vec<Card>)> {
        let mut positions = Vec::new();
        while !hands[0].is_empty() {
            let mut trick: Vec<Card> = Vec::new();
            for seat in 0..4 {
                positions.push((hands, trick.clone()));
                let mut legal = match trick.first() {
                    None => hands[0],
                    Some(lead) => hands[seat].following_suit(lead.suit()),
                };
                trick.push(legal.pick_rng(rng, 1).unwrap().max().unwrap());
            }
            for seat in 0..4 {
                hands[seat] -= Cards::singleton(trick[seat]);
            }
            // Who leads next does not matter here.
            hands.rotate_left(rng.gen_range(0..4));
        }
        positions
    }

    #[test]
    fn every_play_comes_once_with_one_card_of_each_run() {
        let mut rng = SmallRng::seed_from_u64(1);
        for _ in 0..200 {
            let size = rng.gen_range(1..=13);
            let trump = [None, Some(Suit::Spades)][rng.gen_range(0..2)];
            let dealt = deal(&mut rng, size);
            for (hands, trick) in positions(&mut rng, dealt) {
                let all = hands[0] + hands[1] + hands[2] + hands[3];
                let legal = match trick.first() {
                    None => hands[0],
                    Some(lead) => hands[trick.len()].following_suit(lead.suit()),
                };
                let plays: Vec<Card> = likely_plays(trump, &hands, &trick).collect();
                let set = plays
                    .iter()
                    .fold(Cards::EMPTY, |set, &card| set.insert(card));
                assert_eq!(plays.len(), set.len(), "{plays:?}");
                assert_eq!(legal.lowest_of_sequences(all), set);
                let weights = made_up_weights(size as u64);
                let fitted: Vec<Card> = fitted::fitted_plays(trump, &hands, &trick, &weights).collect();
                let fitted_set = fitted.iter().fold(Cards::EMPTY, |set, &card| set.insert(card));
                assert_eq!((fitted.len(), fitted_set), (plays.len(), set));
            }
        }
    }

    /// How good a player `likely_plays` is on its own, playing its first
    /// card, by the tricks that card loses to the best one, as the double
    /// dummy solver counts them, at each seat to a trick.
    #[test]
    #[ignore]
    fn report_how_good_the_first_play_is() {
        let mut rng = SmallRng::seed_from_u64(2);
        let mut best = [0; 4];
        let mut lost = [0; 4];
        let mut seen = [0; 4];
        for _ in 0..200 {
            let trump = [None, Some(Suit::Hearts)][rng.gen_range(0..2)];
            let dealt = deal(&mut rng, 7);
            for (hands, trick) in positions(&mut rng, dealt) {
                let seat = trick.len();
                let each = DoubleDummy::new(trump).tricks_after_each(hands, &trick);
                let tricks = |card| each.iter().find(|&&(c, _)| c == card).unwrap().1;
                let first = tricks(first(trump, &hands, &trick));
                // The leaders want the most tricks, and the others the least.
                let target = if seat % 2 == 0 {
                    each.iter().map(|&(_, t)| t).max().unwrap()
                } else {
                    each.iter().map(|&(_, t)| t).min().unwrap()
                };
                seen[seat] += 1;
                best[seat] += (first == target) as usize;
                lost[seat] += first.abs_diff(target);
            }
        }
        for seat in 0..4 {
            println!(
                "seat {seat}: best {:.3} of the time, {:.3} tricks lost a play",
                best[seat] as f64 / seen[seat] as f64,
                lost[seat] as f64 / seen[seat] as f64
            );
        }
    }

    /// Weights made up from `seed`, for orders that are not fitted.
    fn made_up_weights(seed: u64) -> Vec<f32> {
        let mut rng = SmallRng::seed_from_u64(seed);
        (0..fitted::ENTRIES).map(|_| rng.gen_range(-1.0f32..1.0)).collect()
    }

    #[test]
    fn a_fitted_order_finds_the_same_tricks() {
        let mut rng = SmallRng::seed_from_u64(3);
        for i in 0..40 {
            let trump = [None, Some(Suit::Hearts)][i % 2];
            let dealt = deal(&mut rng, 6);
            let mut tiers = DoubleDummy::new(trump);
            let mut fitted = DoubleDummy::new(trump).with_play_weights(made_up_weights(i as u64));
            for (hands, trick) in positions(&mut rng, dealt).into_iter().step_by(5) {
                assert_eq!(
                    tiers.tricks_after_each(hands, &trick),
                    fitted.tricks_after_each(hands, &trick)
                );
            }
        }
    }

    fn var(name: &str, default: u64) -> u64 {
        std::env::var(name)
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(default)
    }

    fn pair(card: Card) -> [u8; 2] {
        [card.suit() as u8, card.rank()]
    }

    /// Writes positions of random deals, with the double-dummy tricks after
    /// each play worth trying, one JSON object per line, to the file
    /// `PLAY_ORDER_DATA`: the deal's number, the trumps (0 for clubs to 3 for
    /// spades, or null), the hands renumbered among the cards left (as the
    /// solver keeps them) in the order of play from the leader of the trick,
    /// the cards played to the trick so far, and for each play that
    /// `likely_plays` gives, the tricks that the side that led the trick
    /// takes after it with best play.  Positions with only one such play are
    /// left out.  There are `PLAY_ORDER_DEALS` (1000) deals, from the seed
    /// `PLAY_ORDER_SEED` (1), each of from 2 to `PLAY_ORDER_TRICKS` (8)
    /// tricks, played through at random.  It uses every core.
    #[test]
    #[ignore]
    fn collect_play_order_data() {
        let path = std::env::var("PLAY_ORDER_DATA").expect("PLAY_ORDER_DATA");
        let deals = var("PLAY_ORDER_DEALS", 1000) as usize;
        let seed = var("PLAY_ORDER_SEED", 1);
        let most = var("PLAY_ORDER_TRICKS", 8) as usize;
        let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
        let next = std::sync::atomic::AtomicUsize::new(0);
        let lines = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| loop {
                    let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    if i >= deals {
                        break;
                    }
                    let mut rng = SmallRng::seed_from_u64(seed * 1_000_000 + i as u64);
                    let size = rng.gen_range(2..=most);
                    let trump = [None, Some(Suit::Clubs), Some(Suit::Diamonds), Some(Suit::Hearts), Some(Suit::Spades)]
                        [rng.gen_range(0..5)];
                    let mut solver = DoubleDummy::new(trump);
                    let dealt = deal(&mut rng, size);
                    let mut found = Vec::new();
                    for (hands, trick) in positions(&mut rng, dealt) {
                        let all = hands[0] + hands[1] + hands[2] + hands[3];
                        let renumber = |card| Cards::singleton(card).renumbered_among(all).max().unwrap();
                        let hands = hands.map(|hand| hand.renumbered_among(all));
                        let trick: Vec<Card> = trick.into_iter().map(renumber).collect();
                        let plays = fitted::Position::new(trump, &hands, &trick).plays();
                        if plays.len() < 2 {
                            continue;
                        }
                        let each = solver.tricks_after_each(hands, &trick);
                        let tricks = |card| each.iter().find(|&&(c, _)| c == card).unwrap().1;
                        let line = serde_json::json!({
                            "deal": i,
                            "trump": trump.map(|suit| suit as u8),
                            "hands": hands.map(|hand| hand.map(pair).collect::<Vec<_>>()),
                            "trick": trick.iter().map(|&card| pair(card)).collect::<Vec<_>>(),
                            "plays": plays.map(|card| (pair(card), tricks(card))).collect::<Vec<_>>(),
                        });
                        found.push(line.to_string());
                    }
                    lines.lock().unwrap().push((i, found));
                });
            }
        });
        let mut lines = lines.into_inner().unwrap();
        lines.sort();
        let text: String = lines
            .into_iter()
            .flat_map(|(_, found)| found)
            .map(|line| line + "\n")
            .collect();
        std::fs::write(path, text).unwrap();
    }

    /// A position from the data that `collect_play_order_data` writes.
    struct Labelled {
        deal: usize,
        trump: Option<Suit>,
        hands: [Cards; 4],
        trick: Vec<Card>,
        /// Each play worth trying, and whether it is one of the best for the
        /// side that plays it.
        plays: Vec<(Card, bool)>,
    }

    /// The positions in the files `paths`, comma-separated.
    fn read_positions(paths: &str) -> Vec<Labelled> {
        let card = |pair: &serde_json::Value| {
            Card::new(
                Suit::ALL[pair[0].as_u64().unwrap() as usize],
                pair[1].as_u64().unwrap() as u8,
            )
        };
        let mut positions = Vec::new();
        for path in paths.split(',') {
            let text = std::fs::read_to_string(path.trim()).unwrap();
            for line in text.lines() {
                let json: serde_json::Value = serde_json::from_str(line).unwrap();
                let trick: Vec<Card> = json["trick"].as_array().unwrap().iter().map(card).collect();
                let tricks: Vec<(Card, u64)> = json["plays"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|play| (card(&play[0]), play[1].as_u64().unwrap()))
                    .collect();
                // The side that led the trick wants the most tricks, and the
                // other side the fewest.
                let leaders = trick.len().is_multiple_of(2);
                let values = tricks.iter().map(|&(_, t)| t);
                let target = if leaders { values.max() } else { values.min() }.unwrap();
                let hands = [0, 1, 2, 3].map(|i| {
                    json["hands"][i]
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(card)
                        .fold(Cards::EMPTY, Cards::insert)
                });
                positions.push(Labelled {
                    deal: json["deal"].as_u64().unwrap() as usize,
                    trump: json["trump"].as_u64().map(|s| Suit::ALL[s as usize]),
                    hands,
                    trick,
                    plays: tricks.into_iter().map(|(c, t)| (c, t == target)).collect(),
                });
            }
        }
        positions
    }

    /// The entries that each play of each of `positions` looks up, and
    /// whether it is one of the best.
    type Prepared = Vec<Vec<([usize; fitted::TUPLES.len()], bool)>>;

    fn prepare(positions: &[&Labelled]) -> Prepared {
        positions
            .iter()
            .map(|p| {
                let position = fitted::Position::new(p.trump, &p.hands, &p.trick);
                p.plays
                    .iter()
                    .map(|&(card, best)| (position.entries(card), best))
                    .collect()
            })
            .collect()
    }

    /// Weights that make the best plays the likeliest, fitted to `data` by
    /// `steps` steps of Adam on the mean log of the chance that a play
    /// chosen with odds that grow exponentially with its score is one of the
    /// best, less `penalty` times the size of the weights.
    fn fit_order(data: &Prepared, penalty: f64, steps: usize) -> Vec<f32> {
        let size = fitted::ENTRIES;
        let mut weights = vec![0.0f64; size];
        let (mut first, mut second) = (vec![0.0; size], vec![0.0; size]);
        for step in 1..=steps {
            let mut gradient: Vec<f64> = weights.iter().map(|w| -penalty * w).collect();
            for plays in data {
                let scores: Vec<f64> = plays
                    .iter()
                    .map(|(entries, _)| entries.iter().map(|&e| weights[e]).sum())
                    .collect();
                let most = scores.iter().copied().fold(f64::MIN, f64::max);
                let odds: Vec<f64> = scores.iter().map(|s| (s - most).exp()).collect();
                let sum: f64 = odds.iter().sum();
                let best: f64 = odds.iter().zip(plays).filter(|(_, p)| p.1).map(|(o, _)| o).sum();
                for (o, (entries, is_best)) in odds.iter().zip(plays) {
                    let wanted = if *is_best { o / best } else { 0.0 };
                    let g = (wanted - o / sum) / data.len() as f64;
                    for &e in entries {
                        gradient[e] += g;
                    }
                }
            }
            let t = step as f64;
            let rate = 0.05 * (1.0 - (t - 1.0) / steps as f64);
            for j in 0..size {
                first[j] = 0.9 * first[j] + 0.1 * gradient[j];
                second[j] = 0.999 * second[j] + 0.001 * gradient[j] * gradient[j];
                let a = first[j] / (1.0 - 0.9f64.powf(t));
                let b = second[j] / (1.0 - 0.999f64.powf(t));
                weights[j] += rate * a / (b.sqrt() + 1e-8);
            }
        }
        weights.into_iter().map(|w| w as f32).collect()
    }

    /// How often the play that `weights` scores highest is one of the best,
    /// over `data`.
    fn first_is_best(data: &Prepared, weights: &[f32]) -> f64 {
        let hits = data
            .iter()
            .filter(|plays| {
                let score = |p: &([usize; fitted::TUPLES.len()], bool)| -> f32 {
                    p.0.iter().map(|&e| weights[e]).sum()
                };
                let mut top = &plays[0];
                for play in &plays[1..] {
                    if score(play) > score(top) {
                        top = play;
                    }
                }
                top.1
            })
            .count();
        hits as f64 / data.len() as f64
    }

    /// Fits the weights of `fitted` to the data in the files
    /// `PLAY_ORDER_DATA`, comma-separated, picking the penalty on the size of
    /// the weights by five-fold cross-validation over the deals, and says how
    /// often the first play is one of the best on the data in
    /// `PLAY_ORDER_TEST`, by the fitted order and by the tiers of
    /// `likely_plays`, at each seat.  Writes the weights, one a line, to the
    /// file `PLAY_ORDER_WEIGHTS` if it is set.  `PLAY_ORDER_STEPS` (300) is
    /// the number of steps of each fit.
    #[test]
    #[ignore]
    fn fit_play_order() {
        let train = read_positions(&std::env::var("PLAY_ORDER_DATA").expect("PLAY_ORDER_DATA"));
        let test = read_positions(&std::env::var("PLAY_ORDER_TEST").expect("PLAY_ORDER_TEST"));
        let steps = var("PLAY_ORDER_STEPS", 300) as usize;
        let all: Vec<&Labelled> = train.iter().collect();
        let mut best = (f64::MIN, 0.0);
        for penalty in [0.0, 1e-5, 1e-4, 1e-3] {
            let hits: f64 = std::thread::scope(|scope| {
                let folds: Vec<_> = (0..5)
                    .map(|fold| {
                        let all = &all;
                        scope.spawn(move || {
                            let split = |held: bool| -> Vec<&Labelled> {
                                all.iter().copied().filter(|p| (p.deal % 5 == fold) == held).collect()
                            };
                            let held = prepare(&split(true));
                            let weights = fit_order(&prepare(&split(false)), penalty, steps);
                            first_is_best(&held, &weights) * held.len() as f64
                        })
                    })
                    .collect();
                folds.into_iter().map(|f| f.join().unwrap()).sum()
            });
            let rate = hits / all.len() as f64;
            println!("penalty {penalty}: first play best {rate:.4} of the time, held out");
            if rate > best.0 {
                best = (rate, penalty);
            }
        }
        let weights = fit_order(&prepare(&all), best.1, steps);
        for seat in 0..4 {
            let at: Vec<&Labelled> = test.iter().filter(|p| p.trick.len() == seat).collect();
            let tiers = at
                .iter()
                .filter(|p| {
                    let first = likely_plays(p.trump, &p.hands, &p.trick).next().unwrap();
                    p.plays.iter().any(|&(card, best)| card == first && best)
                })
                .count() as f64
                / at.len() as f64;
            println!(
                "seat {seat}, {} test positions: first play best {:.4} fitted, {tiers:.4} tiers",
                at.len(),
                first_is_best(&prepare(&at), &weights),
            );
        }
        if let Ok(path) = std::env::var("PLAY_ORDER_WEIGHTS") {
            let text: String = weights.iter().map(|w| format!("{w}\n")).collect();
            std::fs::write(path, text).unwrap();
        }
    }

    /// How long the solver takes to solve `PLAY_ORDER_SPEED_DEALS` (20) deals
    /// of `PLAY_ORDER_TRICKS` (9) tricks, by the tiers of `likely_plays` and by
    /// the order of the weights in the file `PLAY_ORDER_WEIGHTS`, checking that
    /// both find the same tricks.
    #[test]
    #[ignore]
    fn report_play_order_speed() {
        let text = std::fs::read_to_string(std::env::var("PLAY_ORDER_WEIGHTS").expect("PLAY_ORDER_WEIGHTS")).unwrap();
        let weights: Vec<f32> = text.lines().map(|line| line.parse().unwrap()).collect();
        let deals = var("PLAY_ORDER_SPEED_DEALS", 20);
        let size = var("PLAY_ORDER_TRICKS", 9) as usize;
        let mut rng = SmallRng::seed_from_u64(4);
        let dealt: Vec<(Option<Suit>, [Cards; 4])> = (0..deals)
            .map(|i| ([None, Some(Suit::Spades)][i as usize % 2], deal(&mut rng, size)))
            .collect();
        let mut tricks = [0; 2];
        for (i, name) in ["tiers", "fitted"].iter().enumerate() {
            let started = std::time::Instant::now();
            for &(trump, hands) in &dealt {
                let mut solver = DoubleDummy::new(trump);
                if i == 1 {
                    solver = solver.with_play_weights(weights.clone());
                }
                tricks[i] += solver.tricks(hands);
            }
            let seconds = started.elapsed().as_secs_f64() / deals as f64;
            println!("{name}: {seconds:.4} s a deal");
        }
        assert_eq!(tricks[0], tricks[1]);
    }
}
