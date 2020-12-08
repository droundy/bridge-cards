#![deny(missing_docs)]
//! This crate defines a type [`Card`] for an individual card ([`Card`]) and
//! another type [`Cards`] for a deck or hand of cards.

/// A single card
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Card {
    offset: u8,
}

impl std::fmt::Debug for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rank = self.rank();
        match rank {
            11 => write!(f, "J{:?}", self.suit()),
            12 => write!(f, "Q{:?}", self.suit()),
            13 => write!(f, "K{:?}", self.suit()),
            14 => write!(f, "A{:?}", self.suit()),
            _ => write!(f, "{}{:?}", rank, self.suit()),
        }
    }
}
impl std::fmt::Debug for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Suit::Clubs => write!(f, "C"),
            Suit::Diamonds => write!(f, "D"),
            Suit::Hearts => write!(f, "H"),
            Suit::Spades => write!(f, "S"),
        }
    }
}

#[test]
fn debug() {
    assert_eq!(&format!("{:?}", Card::new(Suit::Hearts, 4)), "4H");
    assert_eq!(&format!("{:?}", Card::SA), "AS");
}

/// This module defines card constants
impl Card {
    /// Create a new card with given rank and suit.
    pub const fn new(suit: Suit, rank: u8) -> Card {
        // debug_assert!(rank < 15);
        // debug_assert!(rank > 1);
        Card {
            offset: rank + 16 * suit as u8,
        }
    }
    /// What is the Suit?
    pub const fn suit(self) -> Suit {
        match self.offset >> 4 {
            0 => Suit::Clubs,
            1 => Suit::Diamonds,
            2 => Suit::Hearts,
            _ => Suit::Spades,
        }
    }
    /// What is my rank?
    pub const fn rank(self) -> u8 {
        self.offset % 16
    }
    /// 2 of Clubs
    pub const C2: Card = Card::new(Suit::Clubs, 2);
    /// 3 of Clubs
    pub const C3: Card = Card::new(Suit::Clubs, 3);
    /// 4 of Clubs
    pub const C4: Card = Card::new(Suit::Clubs, 4);
    /// 5 of Clubs
    pub const C5: Card = Card::new(Suit::Clubs, 5);
    /// 6 of Clubs
    pub const C6: Card = Card::new(Suit::Clubs, 6);
    /// 7 of Clubs
    pub const C7: Card = Card::new(Suit::Clubs, 7);
    /// 8 of Clubs
    pub const C8: Card = Card::new(Suit::Clubs, 8);
    /// 9 of Clubs
    pub const C9: Card = Card::new(Suit::Clubs, 9);
    /// 10 of Clubs
    pub const C10: Card = Card::new(Suit::Clubs, 10);
    /// Jack of Clubs
    pub const CJ: Card = Card::new(Suit::Clubs, 11);
    /// Queen of Clubs
    pub const CQ: Card = Card::new(Suit::Clubs, 12);
    /// King of Clubs
    pub const CK: Card = Card::new(Suit::Clubs, 13);
    /// Ace of Clubs
    pub const CA: Card = Card::new(Suit::Clubs, 14);

    /// 2 of Diamonds
    pub const D2: Card = Card::new(Suit::Diamonds, 2);
    /// 3 of Diamonds
    pub const D3: Card = Card::new(Suit::Diamonds, 3);
    /// 4 of Diamonds
    pub const D4: Card = Card::new(Suit::Diamonds, 4);
    /// 5 of Diamonds
    pub const D5: Card = Card::new(Suit::Diamonds, 5);
    /// 6 of Diamonds
    pub const D6: Card = Card::new(Suit::Diamonds, 6);
    /// 7 of Diamonds
    pub const D7: Card = Card::new(Suit::Diamonds, 7);
    /// 8 of Diamonds
    pub const D8: Card = Card::new(Suit::Diamonds, 8);
    /// 9 of Diamonds
    pub const D9: Card = Card::new(Suit::Diamonds, 9);
    /// 10 of Diamonds
    pub const D10: Card = Card::new(Suit::Diamonds, 10);
    /// Jack of Diamonds
    pub const DJ: Card = Card::new(Suit::Diamonds, 11);
    /// Queen of Diamonds
    pub const DQ: Card = Card::new(Suit::Diamonds, 12);
    /// King of Diamonds
    pub const DK: Card = Card::new(Suit::Diamonds, 13);
    /// Ace of Diamonds
    pub const DA: Card = Card::new(Suit::Diamonds, 14);

    /// 2 of Hearts
    pub const H2: Card = Card::new(Suit::Hearts, 2);
    /// 3 of Hearts
    pub const H3: Card = Card::new(Suit::Hearts, 3);
    /// 4 of Hearts
    pub const H4: Card = Card::new(Suit::Hearts, 4);
    /// 5 of Hearts
    pub const H5: Card = Card::new(Suit::Hearts, 5);
    /// 6 of Hearts
    pub const H6: Card = Card::new(Suit::Hearts, 6);
    /// 7 of Hearts
    pub const H7: Card = Card::new(Suit::Hearts, 7);
    /// 8 of Hearts
    pub const H8: Card = Card::new(Suit::Hearts, 8);
    /// 9 of Hearts
    pub const H9: Card = Card::new(Suit::Hearts, 9);
    /// 10 of Hearts
    pub const H10: Card = Card::new(Suit::Hearts, 10);
    /// Jack of Hearts
    pub const HJ: Card = Card::new(Suit::Hearts, 11);
    /// Queen of Hearts
    pub const HQ: Card = Card::new(Suit::Hearts, 12);
    /// King of Hearts
    pub const HK: Card = Card::new(Suit::Hearts, 13);
    /// Ace of Hearts
    pub const HA: Card = Card::new(Suit::Hearts, 14);

    /// 2 of Spades
    pub const S2: Card = Card::new(Suit::Spades, 2);
    /// 3 of Spades
    pub const S3: Card = Card::new(Suit::Spades, 3);
    /// 4 of Spades
    pub const S4: Card = Card::new(Suit::Spades, 4);
    /// 5 of Spades
    pub const S5: Card = Card::new(Suit::Spades, 5);
    /// 6 of Spades
    pub const S6: Card = Card::new(Suit::Spades, 6);
    /// 7 of Spades
    pub const S7: Card = Card::new(Suit::Spades, 7);
    /// 8 of Spades
    pub const S8: Card = Card::new(Suit::Spades, 8);
    /// 9 of Spades
    pub const S9: Card = Card::new(Suit::Spades, 9);
    /// 10 of Spades
    pub const S10: Card = Card::new(Suit::Spades, 10);
    /// Jack of Spades
    pub const SJ: Card = Card::new(Suit::Spades, 11);
    /// Queen of Spades
    pub const SQ: Card = Card::new(Suit::Spades, 12);
    /// King of Spades
    pub const SK: Card = Card::new(Suit::Spades, 13);
    /// Ace of Spades
    pub const SA: Card = Card::new(Suit::Spades, 14);
}

/// The four suits
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Suit {
    /// Clubs
    Clubs = 0,
    /// Diamonds
    Diamonds = 1,
    /// Hearts
    Hearts = 2,
    /// Spades
    Spades = 3,
}

/// A deck or hand of cards
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Cards {
    bits: u64,
}

impl Cards {
    /// How many cards are there?
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

    /// Join two stacks of cards together
    pub fn union(&self, cards: Cards) -> Cards {
        Cards {
            bits: self.bits | cards.bits,
        }
    }

    /// Randomly pick `num` cards to remove from the deck.
    /// Returns `None` only if there aren't enough cards.
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

    /// All 52 cards.
    pub const ALL: Cards = Cards {
        bits: 0x7ffc + (0x7ffc << 16) + (0x7ffc << 32) + (0x7ffc << 48),
    };
    /// A deck or hand with no cards in it.
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
fn all_cards() {
    for c in Cards::ALL {
        println!("c: {:?}", c);
    }
    assert_eq!(Cards::ALL.len(), 52);
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
