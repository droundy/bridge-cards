//! A small search of one suit, for [`super::Table::SuitPlay`]: the tricks
//! that the player's side takes in a suit when it is played out from now,
//! both sides playing their best, as though it were notrump and every hand
//! that still holds the suit had an entry whenever its side wins.  Each hand
//! holds some ranks of the suit, as bits (bit 0 for the 2, bit 12 for the
//! ace), and the hands count clockwise from the player, whose side is hands
//! 0 and 2.  The winner of a trick leads the next; a leader with none of the
//! suit passes the lead to its partner, or else to the next hand clockwise
//! that has some.  A hand with none of the suit plays nothing to a trick.

use std::collections::HashMap;

/// The ranks of the suit that each hand holds, the player first.
pub(super) type Hands = [u16; 4];

/// A card played to a trick: the hand and its rank, or no rank for a hand
/// with none of the suit.
type Played = (usize, Option<u16>);

/// The search, with what it has worked out of positions at the start of a
/// trick, which every split and every card of a view can share.
#[derive(Default)]
pub(super) struct Search {
    memo: HashMap<u64, u8>,
}

impl Search {
    /// The tricks that the player's side takes from the start of a trick that
    /// `leader` leads (or the hand after it that can).
    pub(super) fn led_by(&mut self, hands: Hands, leader: usize) -> u8 {
        if hands.iter().all(|&h| h == 0) {
            return 0;
        }
        let leader = [0, 2, 1, 3]
            .into_iter()
            .map(|n| (leader + n) % 4)
            .find(|&h| hands[h] != 0)
            .unwrap();
        let key = hands.iter().fold(0u64, |key, &h| key << 13 | u64::from(h)) << 2 | leader as u64;
        if let Some(&value) = self.memo.get(&key) {
            return value;
        }
        let value = self.within(hands, leader, &mut [(0, None); 4], 0);
        self.memo.insert(key, value);
        value
    }

    /// The tricks that the player's side takes when the hands are `hands`
    /// and the current trick, which `leader` led, has `trick[..n]`.
    pub(super) fn within(&mut self, hands: Hands, leader: usize, trick: &mut [Played; 4], n: usize) -> u8 {
        if n == 4 {
            let (winner, _) = trick.iter().filter_map(|&(h, r)| Some((h, r?))).max_by_key(|&(_, r)| r).unwrap();
            return u8::from(winner.is_multiple_of(2)) + self.led_by(hands, winner);
        }
        let hand = (leader + n) % 4;
        if hands[hand] == 0 {
            trick[n] = (hand, None);
            return self.within(hands, leader, trick, n + 1);
        }
        // Cards that no other card still out, nor one in the trick, lies
        // between are the same card: try the lowest of each run.
        let out = hands.iter().fold(0, |all, &h| all | h) | trick[..n].iter().filter_map(|p| p.1).fold(0, |all, r| all | 1 << r);
        let ours = hand.is_multiple_of(2);
        let mut best: Option<u8> = None;
        for rank in 0..13 {
            if hands[hand] >> rank & 1 == 0 {
                continue;
            }
            let below = (0..rank).rev().find(|&r| out >> r & 1 == 1);
            if below.is_some_and(|r| hands[hand] >> r & 1 == 1) {
                continue;
            }
            let mut after = hands;
            after[hand] &= !(1 << rank);
            trick[n] = (hand, Some(rank));
            let value = self.within(after, leader, trick, n + 1);
            best = Some(match best {
                None => value,
                Some(b) if ours => b.max(value),
                Some(b) => b.min(value),
            });
        }
        best.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const A: u16 = 1 << 12;
    const K: u16 = 1 << 11;
    const Q: u16 = 1 << 10;
    const J: u16 = 1 << 9;
    const T: u16 = 1 << 8;
    const X: u16 = 1;
    const Y: u16 = 2;

    #[test]
    fn top_cards_take_their_tricks() {
        let mut search = Search::default();
        // AK opposite two small, the opponents with three each: two tricks.
        assert_eq!(search.led_by([A | K, X | Y, 1 << 2 | 1 << 3 | 1 << 4, Q | J | T], 0), 2);
    }

    #[test]
    fn a_finesse_works_only_through_the_king() {
        let mut search = Search::default();
        // AQ opposite two small, the king guarded: two tricks if it sits
        // under the AQ and the lead comes from the small cards, one if the
        // lead comes from the AQ, and one if the king sits over it.
        let left = [X | Y, K | 1 << 2, A | Q, 1 << 3 | 1 << 4];
        assert_eq!(search.led_by(left, 0), 2);
        assert_eq!(search.led_by(left, 2), 1);
        let right = [X | Y, 1 << 2 | 1 << 4, A | Q, K | 1 << 3];
        assert_eq!(search.led_by(right, 0), 1);
    }
}
