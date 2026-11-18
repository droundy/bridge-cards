//! Small numbers made of a few of the bits of a set of cards, for indexing
//! the tables of an n-tuple network: a model that scores a play by adding up
//! entries of tables, each looked up by a few plain facts about it, such as
//! which cards a hand holds near the card played.
//!
//! Each number is a few shifts and masks of the bits of [`Cards`], which this
//! module can see because it is part of the crate.

use crate::{Card, Cards, Suit};

impl Cards {
    /// The ranks of `suit` that these cards hold, as bits: the deuce is bit
    /// 2 and the ace bit 14.
    const fn suit_bits(self, suit: Suit) -> u64 {
        (self.bits >> (16 * suit as u32)) & 0x7fff
    }

    /// Which of the `n` ranks just above `card` these cards hold, as the bits
    /// of a number below `2^n`: the rank just above is bit 0.  Ranks above the
    /// ace are never held.
    pub const fn above(self, card: Card, n: u32) -> usize {
        let suit_bits = self.suit_bits(card.suit());
        ((suit_bits >> (card.rank() as u32 + 1)) & ((1 << n) - 1)) as usize
    }

    /// Which of the `n` ranks just below `card` these cards hold, as the bits
    /// of a number below `2^n`: the rank just below is bit `n - 1`.  Ranks
    /// below the deuce are never held.
    pub const fn below(self, card: Card, n: u32) -> usize {
        let suit_bits = self.suit_bits(card.suit());
        (((suit_bits << n) >> card.rank()) & ((1 << n) - 1)) as usize
    }

    /// Which of the `n` highest ranks of `suit` these cards hold, as the bits
    /// of a number below `2^n`: the ace is bit `n - 1`.
    pub const fn top_ranks(self, suit: Suit, n: u32) -> usize {
        ((self.suit_bits(suit) >> (15 - n)) & ((1 << n) - 1)) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Whether `hand` holds the card of `suit` of `rank`, which may be no card.
    fn holds(hand: Cards, suit: Suit, rank: i32) -> bool {
        (2..=14).contains(&rank) && hand.contains(Card::new(suit, rank as u8))
    }

    /// The bits that a number is made of, built one card at a time.
    fn number(bits: impl Iterator<Item = bool>) -> usize {
        bits.enumerate().map(|(i, b)| (b as usize) << i).sum()
    }

    #[test]
    fn the_numbers_are_the_cards_held() {
        let mut deck = Cards::ALL;
        for _ in 0..200 {
            let hand = deck.pick(13).unwrap();
            deck = Cards::ALL;
            for card in Cards::ALL {
                let (suit, rank) = (card.suit(), card.rank() as i32);
                for n in 1..=5 {
                    let above = number((1..=n).map(|i| holds(hand, suit, rank + i)));
                    let below = number((1..=n).rev().map(|i| holds(hand, suit, rank - i)));
                    assert_eq!(hand.above(card, n as u32), above, "{hand:?} {card:?} {n}");
                    assert_eq!(hand.below(card, n as u32), below, "{hand:?} {card:?} {n}");
                    let top = number((0..n).rev().map(|i| holds(hand, suit, 14 - i)));
                    assert_eq!(hand.top_ranks(suit, n as u32), top, "{hand:?} {suit:?} {n}");
                }
            }
        }
    }
}
