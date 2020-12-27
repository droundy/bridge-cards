#![deny(missing_docs)]
//! This crate defines a type [`Card`] for an individual card ([`Card`]) and
//! another type [`Cards`] for a deck or hand of cards.

// pub mod simd;

use rand::Rng;

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
    /// What is my rank called?
    pub const fn rankname(self) -> &'static str {
        match self.rank() {
            2 => "2",
            3 => "3",
            4 => "4",
            5 => "5",
            6 => "6",
            7 => "7",
            8 => "8",
            9 => "9",
            10 => "T",
            11 => "J",
            12 => "Q",
            13 => "K",
            14 => "A",
            _ => "?",
        }
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

impl Card {
    fn unicode(self) -> char {
        let start = 0x1F0D1 - (self.suit() as u32) * 16;
        let shift = if self.rank() == 14 {
            1
        } else {
            self.rank() as u32
        };
        use std::convert::TryFrom;
        char::try_from(start + shift - 1).unwrap()
    }
}

#[test]
fn unicode_test() {
    assert_eq!('🃑', Card::CA.unicode());
    assert_eq!('🃒', Card::C2.unicode());

    assert_eq!('🃁', Card::DA.unicode());
    assert_eq!('🃂', Card::D2.unicode());

    assert_eq!('🂱', Card::HA.unicode());
    assert_eq!('🂲', Card::H2.unicode());

    assert_eq!('🂡', Card::SA.unicode());
    assert_eq!('🂢', Card::S2.unicode());
}

impl std::fmt::Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.unicode())
    }
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

impl Suit {
    /// The name of the suit
    pub fn name(self) -> &'static str {
        match self {
            Suit::Clubs => "clubs",
            Suit::Diamonds => "diamonds",
            Suit::Hearts => "hearts",
            Suit::Spades => "spades",
        }
    }
    fn unicode(self) -> char {
        match self {
            Suit::Clubs => '♣',
            Suit::Diamonds => '♦',
            Suit::Hearts => '♥',
            Suit::Spades => '♠',
        }
    }
}

/// A deck or hand of cards
///
/// This data structure assumes that each card can appear only once, as in the
/// game bridge.  If you want to allow more than one deck mixed together you
/// want a different crate.  Because each card can only appear once, `Cards` is
/// `Copy` and only takes 64 bits.  This makes it fast and memory efficient.
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
    pub const fn insert(self, card: Card) -> Cards {
        Cards {
            bits: self.bits | (1 << card.offset),
        }
    }

    /// remove a card from the deck or hand
    pub const fn remove(self, card: Card) -> Cards {
        Cards {
            bits: self.bits & !(1 << card.offset),
        }
    }

    /// A deck with one card
    pub const fn singleton(card: Card) -> Cards {
        Cards {
            bits: 1 << card.offset,
        }
    }

    /// Check if we contain a card
    pub const fn contains(&self, card: Card) -> bool {
        self.bits & (1 << card.offset) != 0
    }

    /// Join two stacks of cards together
    pub const fn union(&self, cards: Cards) -> Cards {
        Cards {
            bits: self.bits | cards.bits,
        }
    }

    /// Randomly pick `num` cards to remove from the deck.
    /// Returns `None` only if there aren't enough cards.
    pub fn pick(&mut self, mut num: usize) -> Option<Cards> {
        self.pick_rng(&mut rand::thread_rng(), num)
    }

    /// Randomly pick `num` cards to remove from the deck using specified RNG.
    /// Returns `None` only if there aren't enough cards.
    pub fn pick_rng<R: Rng>(&mut self, rng: &mut R, mut num: usize) -> Option<Cards> {
        let mut bits = self.bits;
        let mut n_left = self.len();
        if num > n_left {
            return None;
        }
        let mut kept = 0;
        let mut given = 0;
        while n_left > 0 {
            if num == 0 {
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

    const fn intersection(self, rhs: Self) -> Self {
        Cards {
            bits: self.bits & rhs.bits,
        }
    }

    /// All 52 cards.
    pub const ALL: Cards = Self::SPADES
        .union(Self::HEARTS)
        .union(Self::DIAMONDS)
        .union(Self::CLUBS);
    /// All club cards.
    pub const CLUBS: Cards = Cards { bits: 0x7ffc };
    /// Just the clubs from this hand
    pub const fn clubs(self) -> Cards {
        self.intersection(Cards::CLUBS)
    }
    /// All diamond cards.
    pub const DIAMONDS: Cards = Cards { bits: 0x7ffc << 16 };
    /// Just the diamonds from this hand
    pub const fn diamonds(self) -> Cards {
        self.intersection(Cards::DIAMONDS)
    }
    /// All heart cards.
    pub const HEARTS: Cards = Cards { bits: 0x7ffc << 32 };
    /// Just the hearts from this hand
    pub const fn hearts(self) -> Cards {
        self.intersection(Cards::HEARTS)
    }
    /// All spade cards.
    pub const SPADES: Cards = Cards { bits: 0x7ffc << 48 };
    /// Just the spades from this hand
    pub const fn spades(self) -> Cards {
        self.intersection(Cards::SPADES)
    }
    ///
    /// A deck or hand with no cards in it.
    pub const EMPTY: Cards = Cards { bits: 0 };

    /// The aces
    pub const ACES: Cards = Cards::EMPTY
        .insert(Card::CA)
        .insert(Card::DA)
        .insert(Card::HA)
        .insert(Card::SA);
    /// Just the aces from this hand
    pub const fn aces(self) -> Cards {
        self.intersection(Cards::ACES)
    }
    /// The kings
    pub const KINGS: Cards = Cards::EMPTY
        .insert(Card::CK)
        .insert(Card::DK)
        .insert(Card::HK)
        .insert(Card::SK);
    /// Just the kins from this hand
    pub const fn kings(self) -> Cards {
        self.intersection(Cards::KINGS)
    }
    /// The queens
    pub const QUEENS: Cards = Cards::EMPTY
        .insert(Card::CQ)
        .insert(Card::DQ)
        .insert(Card::HQ)
        .insert(Card::SQ);
    /// Just the queens from this hand
    pub const fn queens(self) -> Cards {
        self.intersection(Cards::QUEENS)
    }
    /// The jacks
    pub const JACKS: Cards = Cards::EMPTY
        .insert(Card::CJ)
        .insert(Card::DJ)
        .insert(Card::HJ)
        .insert(Card::SJ);
    /// Just the jacks from this hand
    pub const fn jacks(self) -> Cards {
        self.intersection(Cards::JACKS)
    }

    /// High card points
    pub const fn high_card_points(self) -> usize {
        self.aces().len()
            + self.intersection(Cards::ACES.union(Cards::KINGS)).len()
            + self
                .intersection(Cards::ACES.union(Cards::KINGS).union(Cards::QUEENS))
                .len()
            + self
                .intersection(
                    Cards::ACES
                        .union(Cards::KINGS)
                        .union(Cards::QUEENS)
                        .union(Cards::JACKS),
                )
                .len()
    }

    /// Protected high card points
    pub const fn protected_high_card_points(self) -> usize {
        self.aces().len()
            + self.intersection(Cards::ACES.union(Cards::KINGS)).len()
            + self
                .long_suits(2)
                .intersection(Cards::ACES.union(Cards::KINGS).union(Cards::QUEENS))
                .len()
            + self
                .long_suits(3)
                .intersection(
                    Cards::ACES
                        .union(Cards::KINGS)
                        .union(Cards::QUEENS)
                        .union(Cards::JACKS),
                )
                .len()
    }

    /// Long suits
    pub const fn long_suits(self, at_least: usize) -> Cards {
        let spades = self.spades();
        let spades = if spades.len() >= at_least {
            spades
        } else {
            Cards::EMPTY
        };
        let hearts = self.hearts();
        let hearts = if hearts.len() >= at_least {
            hearts
        } else {
            Cards::EMPTY
        };
        let diamonds = self.diamonds();
        let diamonds = if diamonds.len() >= at_least {
            diamonds
        } else {
            Cards::EMPTY
        };
        let clubs = self.clubs();
        let clubs = if clubs.len() >= at_least {
            clubs
        } else {
            Cards::EMPTY
        };
        spades.union(hearts).union(diamonds).union(clubs)
    }

    /// Long card points
    pub const fn long_card_points(self) -> usize {
        let s = if self.spades().len() > 4 {
            self.spades().len() - 4
        } else {
            0
        };
        let h = if self.hearts().len() > 4 {
            self.hearts().len() - 4
        } else {
            0
        };
        let d = if self.diamonds().len() > 4 {
            self.diamonds().len() - 4
        } else {
            0
        };
        let c = if self.clubs().len() > 4 {
            self.clubs().len() - 4
        } else {
            0
        };
        s + h + d + c
    }

    /// Short card points 3 2 1
    pub const fn short_card_points(self) -> usize {
        let s = if self.spades().len() < 3 {
            3 - self.spades().len()
        } else {
            0
        };
        let h = if self.hearts().len() < 3 {
            3 - self.hearts().len()
        } else {
            0
        };
        let d = if self.diamonds().len() < 3 {
            3 - self.diamonds().len()
        } else {
            0
        };
        let c = if self.clubs().len() < 3 {
            3 - self.clubs().len()
        } else {
            0
        };
        s + h + d + c
    }
}

#[test]
fn test_hcp() {
    assert_eq!(40, Cards::ALL.high_card_points());
    assert_eq!(10, Cards::SPADES.high_card_points());
    assert_eq!(10, Cards::HEARTS.high_card_points());
    assert_eq!(10, Cards::DIAMONDS.high_card_points());
    assert_eq!(10, Cards::CLUBS.high_card_points());
    assert_eq!(16, Cards::ACES.high_card_points());
    assert_eq!(12, Cards::KINGS.high_card_points());
    assert_eq!(8, Cards::QUEENS.high_card_points());
    assert_eq!(4, Cards::JACKS.high_card_points());

    assert_eq!(4, Cards::JACKS.len());
    assert_eq!(4, Cards::QUEENS.len());
    assert_eq!(4, Cards::KINGS.len());
    assert_eq!(4, Cards::ACES.len());
}

impl std::ops::Add for Cards {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        self.union(rhs)
    }
}

impl std::ops::AddAssign for Cards {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.union(rhs)
    }
}

impl std::ops::BitAnd for Cards {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        self.intersection(rhs)
    }
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

impl DoubleEndedIterator for Cards {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            None
        } else {
            let next = 63 - self.bits.leading_zeros();
            self.bits = self.bits & !(1 << next);
            Some(Card { offset: next as u8 })
        }
    }
}

#[cfg(feature = "display-as")]
mod display;

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
    cards = cards.insert(Card::C2);
    assert_eq!(cards.len(), 1);
    cards = cards.insert(Card::C4);
    assert_eq!(cards.len(), 2);
    assert!(cards.contains(Card::C4));
    assert!(!cards.contains(Card::C3));
    assert!(cards.contains(Card::C2));
    assert_eq!(cards.next(), Some(Card::C2));
    assert_eq!(cards.next(), Some(Card::C4));
    assert_eq!(cards.next(), None);

    assert_eq!(cards.len(), 0);
    cards = cards.insert(Card::C2);
    assert_eq!(cards.len(), 1);
    cards = cards.insert(Card::C4);
    assert_eq!(cards.len(), 2);

    let two_cards = cards;

    let mut hand = cards.pick(2).unwrap();
    assert_eq!(cards.len(), 0);
    assert_eq!(hand.len(), 2);
    assert_eq!(hand, two_cards);

    let other = hand.pick(1).unwrap();
    assert_eq!(other.union(hand), two_cards);

    cards = Cards::SPADES;
    assert_eq!(Some(Card::SA), cards.next_back());
}
