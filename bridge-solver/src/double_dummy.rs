//! A solver for deals in which every card is known.
//!
//! [`Naive`](crate::Naive) solves those too, when nothing is unknown, but it is
//! built for deals with unknown cards, whose scores are means over samples, so
//! it works out the exact score of every position it reaches.  With every card
//! known, a score is a whole number of tricks, and a question with a yes or no
//! answer, "can the side on lead take at least `target` more tricks?", is much
//! cheaper to answer: a defence that holds the leaders below the target ends
//! the search of a play, whatever else that play might lead to.  The number of
//! tricks is found by asking that question a few times, in a binary search.
//! What each answer shows of a position, a least and a most number of tricks,
//! is kept, so later questions about the same position are answered at once,
//! and so are the many positions in which the side on lead has enough top
//! cards to take the target without any search.

use crate::{trick_winner, PositionHasher, Starting, TrickTaken};
use bridge_deck::{Card, Cards, Suit};
use std::collections::HashMap;
use std::hash::BuildHasherDefault;

/// The least and the most tricks that the side on lead can take in each
/// position, as far as is known.
type Bounds = HashMap<Starting, (u8, u8), BuildHasherDefault<PositionHasher>>;

/// A double-dummy solver: every hand is known to everyone, and everyone plays
/// as well as can be.
#[derive(Clone)]
pub struct DoubleDummy {
    trump: Option<Suit>,
    bounds: Bounds,
}

impl DoubleDummy {
    /// A solver for deals played with `trump` as trumps, or in notrump.
    pub fn new(trump: Option<Suit>) -> Self {
        DoubleDummy {
            trump,
            bounds: Bounds::default(),
        }
    }

    /// The tricks that the side on lead takes with best play by everyone,
    /// where `hands` are the hands in the order of play from the leader, all
    /// of the same size.
    pub fn tricks(&mut self, hands: [Cards; 4]) -> usize {
        let position = Starting {
            hands,
            unknown: Cards::EMPTY,
        };
        position.check();
        let (mut low, mut high) = (0, hands[0].len());
        while low < high {
            let target = (low + high).div_ceil(2);
            if self.at_least(position, target) {
                low = target;
            } else {
                high = target - 1;
            }
        }
        low
    }

    /// The tricks that the side that led this trick takes, with best play by
    /// everyone, after `plays` have been made to it from `hands`, which are
    /// the hands as they were before the trick.  There may be from one to
    /// four plays.
    pub fn tricks_after(&mut self, hands: [Cards; 4], plays: &[Card]) -> usize {
        let n = hands[0].len();
        let (mut low, mut high) = (0, n);
        while low < high {
            let target = (low + high).div_ceil(2);
            let mut trick = [plays[0]; 4];
            trick[..plays.len()].copy_from_slice(plays);
            if self.makes_from(hands, &mut trick, plays.len(), target) {
                low = target;
            } else {
                high = target - 1;
            }
        }
        low
    }

    /// Whether the side on lead in `position` can take at least `target` of
    /// the tricks that are left.
    fn at_least(&mut self, position: Starting, target: usize) -> bool {
        let n = position.hands[0].len();
        if target == 0 {
            return true;
        }
        if target > n {
            return false;
        }
        let (lower, upper) = self
            .bounds
            .get(&position)
            .map_or((0, n), |&(lower, upper)| (lower as usize, upper as usize));
        if target <= lower {
            return true;
        }
        if target > upper {
            return false;
        }
        let sure = self.sure_tricks(position.hands);
        let makes = sure >= target || {
            let mut trick = [Card::S2; 4];
            self.makes_from(position.hands, &mut trick, 0, target)
        };
        let (lower, upper) = if makes {
            (lower.max(target).max(sure), upper)
        } else {
            (lower.max(sure), upper.min(target - 1))
        };
        self.bounds.insert(position, (lower as u8, upper as u8));
        makes
    }

    /// Whether the side that leads this trick, from `hands` as they were
    /// before it, can take at least `target` of the tricks that are left,
    /// once the first `played` cards of `trick` have been played to it.
    fn makes_from(
        &mut self,
        hands: [Cards; 4],
        trick: &mut [Card; 4],
        played: usize,
        target: usize,
    ) -> bool {
        if played == 4 {
            return self.makes_after(hands, *trick, target);
        }
        let all = hands[0] + hands[1] + hands[2] + hands[3];
        let legal = if played == 0 {
            hands[0]
        } else {
            hands[played].following_suit(trick[0].suit())
        };
        // Of cards that touch, any one is as good as another.
        let plays = legal.lowest_of_sequences(all);
        // The leader and their partner want the target made, and the others
        // want it not to be.
        let wants_it = played & 1 == 0;
        // The card most likely to settle the question is tried first, since
        // once one card does, the others need not be tried.
        let first = self.likely_best(&trick[..played], plays, all);
        let rest = first.map_or(plays, |first| plays - Cards::singleton(first));
        for card in first.into_iter().chain(rest) {
            trick[played] = card;
            if self.makes_from(hands, trick, played + 1, target) == wants_it {
                return wants_it;
            }
        }
        !wants_it
    }

    /// Which of the cards `plays` is likely the best play after `trick`, if
    /// one is: a winner to lead, nothing when partner is winning the trick,
    /// and otherwise the cheapest card that would win it.
    fn likely_best(&self, trick: &[Card], mut plays: Cards, all: Cards) -> Option<Card> {
        if trick.is_empty() {
            // The top card of a suit, which wins the trick unless it is
            // ruffed.
            return plays.find(|&card| all.in_suit(card.suit()).max() == Some(card));
        }
        let winner = self.winner_so_far(trick);
        if winner & 1 == trick.len() & 1 {
            // Partner is winning, and the lowest card is tried first anyway.
            return None;
        }
        let winning = trick[winner];
        plays.find(|&card| self.beats(card, winning))
    }

    /// Which of the cards of a trick that is not finished is winning it.
    fn winner_so_far(&self, trick: &[Card]) -> usize {
        let mut winner = 0;
        for (i, &card) in trick.iter().enumerate().skip(1) {
            if self.beats(card, trick[winner]) {
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

    /// Whether the side that led the trick `plays`, from `hands`, takes at
    /// least `target` of the tricks that were left before it.
    fn makes_after(&mut self, hands: [Cards; 4], plays: [Card; 4], target: usize) -> bool {
        let n = hands[0].len();
        let ours = trick_winner(plays, self.trump) & 1 == 0;
        if n == 1 {
            return ours as usize >= target;
        }
        let before = Starting {
            hands,
            unknown: Cards::EMPTY,
        };
        match before.after_unchecked(plays, self.trump) {
            TrickTaken::Us(next) if ours => self.at_least(next, target - 1),
            // The others lead next, and the leaders make their target if
            // the others take no more than the rest.
            TrickTaken::Them(next) => !self.at_least(next, n - target),
            TrickTaken::Us(_) => unreachable!("the trick was won by the other side"),
        }
    }

    /// Tricks that the leader's side takes however the others play: the top
    /// cards of the leader's suits, which win while they last as long as no
    /// one else can ruff them, and then those of partner in a suit that the
    /// leader can lead to them.
    fn sure_tricks(&self, hands: [Cards; 4]) -> usize {
        let all = hands[0] + hands[1] + hands[2] + hands[3];
        let others_can_ruff = |suit: Suit| match self.trump {
            Some(trump) if trump != suit => !(hands[1] + hands[3]).in_suit(trump).is_empty(),
            _ => false,
        };
        // How many of the top cards of `suit` `hand` holds, from the top.
        let tops = |hand: Cards, suit: Suit| {
            let mut left = all.in_suit(suit);
            let mut tops = 0;
            while let Some(top) = left.max() {
                if !hand.contains(top) {
                    break;
                }
                tops += 1;
                left -= Cards::singleton(top);
            }
            tops
        };
        let mut own = 0;
        let mut partners = 0;
        for suit in Suit::ALL {
            if others_can_ruff(suit) {
                continue;
            }
            own += tops(hands[0], suit);
            if !hands[0].in_suit(suit).is_empty() {
                partners = partners.max(tops(hands[2], suit));
            }
        }
        let n = hands[0].len();
        // The leader cashes their own winners first, and then leads to
        // partner, as long as partner has other cards to play to the leader's
        // winners than the ones that win their suit.
        let partner_can_wait = |suit: Suit| {
            !hands[0].in_suit(suit).is_empty()
                && !others_can_ruff(suit)
                && tops(hands[2], suit) == partners
                && (hands[2] - hands[2].in_suit(suit)).len() >= own
        };
        let sure = if partners > 0 && Suit::ALL.iter().any(|&suit| partner_can_wait(suit)) {
            own + partners
        } else {
            own.max(partners)
        };
        sure.min(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Naive;
    use rand::rngs::SmallRng;
    use rand::{Rng, SeedableRng};

    fn deal(rng: &mut SmallRng, size: usize) -> [Cards; 4] {
        let mut deck = Cards::ALL;
        [0; 4].map(|_| deck.pick_rng(rng, size).unwrap())
    }

    #[test]
    fn tricks_agree_with_the_naive_solver() {
        let mut rng = SmallRng::seed_from_u64(7);
        for size in 1..=7 {
            for _ in 0..100 {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Hearts), Some(Suit::Clubs)][rng.gen_range(0..3)];
                let naive = Naive::new(trump)
                    .score(Starting {
                        hands,
                        unknown: Cards::EMPTY,
                    })
                    .mean();
                let mut solver = DoubleDummy::new(trump);
                assert_eq!(naive, solver.tricks(hands) as f64, "{hands:?} {trump:?}");
                // A second question of the same solver is answered from what
                // it learnt the first time, and must agree.
                assert_eq!(naive, solver.tricks(hands) as f64);
            }
        }
    }

    #[test]
    fn tricks_after_plays_agree_with_trying_each_reply() {
        let mut rng = SmallRng::seed_from_u64(8);
        for size in 2..=5 {
            for _ in 0..30 {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Spades)][rng.gen_range(0..2)];
                let mut solver = DoubleDummy::new(trump);
                let whole = solver.tricks(hands);
                // The best lead makes as many tricks as the deal does.
                let best = hands[0]
                    .map(|lead| solver.tricks_after(hands, &[lead]))
                    .max()
                    .unwrap();
                assert_eq!(whole, best, "{hands:?}");
                for lead in hands[0] {
                    let after_lead = solver.tricks_after(hands, &[lead]);
                    // The second hand plays to hold the leaders to as few as
                    // it can.
                    let worst = hands[1]
                        .following_suit(lead.suit())
                        .map(|second| solver.tricks_after(hands, &[lead, second]))
                        .min()
                        .unwrap();
                    assert_eq!(after_lead, worst, "{hands:?} {lead:?}");
                }
            }
        }
    }

    /// How long each solver takes on deals of each size, with the sizes in
    /// `DD_SIZES` (8 to 13 by default), and `Naive` only up to 10.
    #[test]
    #[ignore]
    fn report_the_time_to_solve_a_deal() {
        let sizes: Vec<usize> = std::env::var("DD_SIZES")
            .ok()
            .map(|s| s.split(',').filter_map(|n| n.parse().ok()).collect())
            .unwrap_or((8..=13).collect());
        for size in sizes {
            let mut rng = SmallRng::seed_from_u64(size as u64);
            let mut dd = Vec::new();
            let mut naive = Vec::new();
            for i in 0..8 {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Spades)][i % 2];
                let started = std::time::Instant::now();
                let tricks = DoubleDummy::new(trump).tricks(hands);
                dd.push(started.elapsed().as_secs_f64());
                if size <= 10 {
                    let started = std::time::Instant::now();
                    let score = Naive::new(trump).score(Starting {
                        hands,
                        unknown: Cards::EMPTY,
                    });
                    naive.push(started.elapsed().as_secs_f64());
                    assert_eq!(tricks as f64, score.mean());
                }
            }
            let summary = |times: &mut Vec<f64>| {
                if times.is_empty() {
                    return "-".to_string();
                }
                times.sort_by(f64::total_cmp);
                format!(
                    "mean {:.4} s, most {:.4} s",
                    times.iter().sum::<f64>() / times.len() as f64,
                    times[times.len() - 1]
                )
            };
            println!(
                "{size} cards: double dummy {}; naive {}",
                summary(&mut dd),
                summary(&mut naive)
            );
        }
    }

    #[test]
    fn the_leader_takes_their_top_cards() {
        // In notrump, the leader holds the top three spades and the top two
        // hearts: five sure tricks.  Partner's spades and hearts are lower,
        // so add nothing.
        let hands = ["S AKQ H AK", "S 432 H 43", "S 765 H 65", "S T98 H 87"]
            .map(|hand| hand.parse::<Cards>().unwrap());
        let solver = DoubleDummy::new(None);
        assert_eq!(5, solver.sure_tricks(hands));
        // With hearts trumps and an opponent who holds a trump, the spades are
        // not sure, but the trumps are.
        let solver = DoubleDummy::new(Some(Suit::Hearts));
        assert_eq!(2, solver.sure_tricks(hands));
    }
}
