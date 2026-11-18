//! Which card a player is likely to play, guessed from the cards alone,
//! without looking ahead.
//!
//! [`likely_plays`] puts the cards that the next player to a trick may play in
//! the order in which they are likely to be best.  A search that tries them in
//! that order finds a good play early, and once it has found a play good
//! enough, it need not try the others.

use bridge_deck::{Card, Cards, Suit};

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
}
