
/// A single card
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Card {
    offset: u8,
}

/// This module defines card constants
impl Card {
    pub const fn new(suit: Suit, rank: u8) -> Card {
        // debug_assert!(rank < 15);
        // debug_assert!(rank > 1);
        Card {
            offset: rank + 16 * suit as u8,
        }
    }
    pub const C2: Card = Card::new(Suit::Clubs, 2);
    pub const C3: Card = Card::new(Suit::Clubs, 3);
    pub const C4: Card = Card::new(Suit::Clubs, 4);
    pub const C5: Card = Card::new(Suit::Clubs, 5);
    pub const C6: Card = Card::new(Suit::Clubs, 6);
    pub const C7: Card = Card::new(Suit::Clubs, 7);
    pub const C8: Card = Card::new(Suit::Clubs, 8);
    pub const C9: Card = Card::new(Suit::Clubs, 9);
    pub const C10: Card = Card::new(Suit::Clubs, 10);
    pub const CJ: Card = Card::new(Suit::Clubs, 11);
    pub const CQ: Card = Card::new(Suit::Clubs, 12);
    pub const CK: Card = Card::new(Suit::Clubs, 13);
    pub const CA: Card = Card::new(Suit::Clubs, 14);

    pub const D2: Card = Card::new(Suit::Diamonds, 2);
    pub const D3: Card = Card::new(Suit::Diamonds, 3);
    pub const D4: Card = Card::new(Suit::Diamonds, 4);
    pub const D5: Card = Card::new(Suit::Diamonds, 5);
    pub const D6: Card = Card::new(Suit::Diamonds, 6);
    pub const D7: Card = Card::new(Suit::Diamonds, 7);
    pub const D8: Card = Card::new(Suit::Diamonds, 8);
    pub const D9: Card = Card::new(Suit::Diamonds, 9);
    pub const D10: Card = Card::new(Suit::Diamonds, 10);
    pub const DJ: Card = Card::new(Suit::Diamonds, 11);
    pub const DQ: Card = Card::new(Suit::Diamonds, 12);
    pub const DK: Card = Card::new(Suit::Diamonds, 13);
    pub const DA: Card = Card::new(Suit::Diamonds, 14);

    pub const H2: Card = Card::new(Suit::Hearts, 2);
    pub const H3: Card = Card::new(Suit::Hearts, 3);
    pub const H4: Card = Card::new(Suit::Hearts, 4);
    pub const H5: Card = Card::new(Suit::Hearts, 5);
    pub const H6: Card = Card::new(Suit::Hearts, 6);
    pub const H7: Card = Card::new(Suit::Hearts, 7);
    pub const H8: Card = Card::new(Suit::Hearts, 8);
    pub const H9: Card = Card::new(Suit::Hearts, 9);
    pub const H10: Card = Card::new(Suit::Hearts, 10);
    pub const HJ: Card = Card::new(Suit::Hearts, 11);
    pub const HQ: Card = Card::new(Suit::Hearts, 12);
    pub const HK: Card = Card::new(Suit::Hearts, 13);
    pub const HA: Card = Card::new(Suit::Hearts, 14);

    pub const S2: Card = Card::new(Suit::Spades, 2);
    pub const S3: Card = Card::new(Suit::Spades, 3);
    pub const S4: Card = Card::new(Suit::Spades, 4);
    pub const S5: Card = Card::new(Suit::Spades, 5);
    pub const S6: Card = Card::new(Suit::Spades, 6);
    pub const S7: Card = Card::new(Suit::Spades, 7);
    pub const S8: Card = Card::new(Suit::Spades, 8);
    pub const S9: Card = Card::new(Suit::Spades, 9);
    pub const S10: Card = Card::new(Suit::Spades, 10);
    pub const SJ: Card = Card::new(Suit::Spades, 11);
    pub const SQ: Card = Card::new(Suit::Spades, 12);
    pub const SK: Card = Card::new(Suit::Spades, 13);
    pub const SA: Card = Card::new(Suit::Spades, 14);
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Suit {
    Clubs = 0,
    Diamonds = 1,
    Hearts = 2,
    Spades = 3,
}

/// A deck or hand of cards
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Cards {
    bits: u64,
}

impl Cards {
    pub const fn len(&self) -> usize {
        self.bits.count_ones() as usize
    }

    /// insert a card to the deck or hand
    pub fn insert(&mut self, card: Card) {
        self.bits = self.bits | (1 << card.offset)
    }

    /// Check if we contain a card
    pub fn contains(&self, card: Card) -> bool {
        self.bits & (1 << card.offset) != 0
    }

    pub fn union(&self, cards: Cards) -> Cards {
        Cards {
            bits: self.bits | cards.bits,
        }
    }

    pub fn pick(&mut self, mut num: usize) -> Option<Cards> {
        let mut bits = self.bits;
        let mut n_left = self.len();
        if num > n_left {
            return None;
        }
        let mut kept = 0;
        let mut given = 0;
        let mut rng = rand::thread_rng();
        while n_left > 0 {
            if n_left == num {
                given |= bits;
                break;
            } else if num == 0 {
                kept |= bits;
                break;
            }
            use rand::Rng;
            let chosen = rng.gen::<u64>() & bits;
            if chosen != 0 {
                let num_chosen = chosen.count_ones() as usize;
                if num_chosen <= num {
                    bits &= !chosen;
                    given |= chosen;
                    n_left -= num_chosen;
                    num -= num_chosen;
                } else if num_chosen + num < n_left {
                    bits &= !chosen;
                    kept |= chosen;
                    n_left -= num_chosen;
                }
            }
        }
        self.bits = kept;
        Some(Cards { bits: given })
    }

    pub const ALL: Cards = Cards { bits: 7 };
    pub const EMPTY: Cards = Cards { bits: 0 };
}

impl Iterator for Cards {
    type Item = Card;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            None
        } else {
            let next = self.bits.trailing_zeros();
            self.bits = self.bits & !(1 << next);
            Some(Card { offset: next as u8 })
        }
    }
}

#[test]
fn iterate() {
    let mut cards = Cards::EMPTY;
    assert_eq!(cards.next(), None);
    assert_eq!(cards.len(), 0);
    cards.insert(Card::C2);
    assert_eq!(cards.len(), 1);
    cards.insert(Card::C4);
    assert_eq!(cards.len(), 2);
    assert!(cards.contains(Card::C4));
    assert!(!cards.contains(Card::C3));
    assert!(cards.contains(Card::C2));
    assert_eq!(cards.next(), Some(Card::C2));
    assert_eq!(cards.next(), Some(Card::C4));
    assert_eq!(cards.next(), None);

    assert_eq!(cards.len(), 0);
    cards.insert(Card::C2);
    assert_eq!(cards.len(), 1);
    cards.insert(Card::C4);
    assert_eq!(cards.len(), 2);

    let two_cards = cards;

    let mut hand = cards.pick(2).unwrap();
    assert_eq!(cards.len(), 0);
    assert_eq!(hand.len(), 2);
    assert_eq!(hand, two_cards);

    let other = hand.pick(1).unwrap();
    assert_eq!(other.union(hand), two_cards);
}
