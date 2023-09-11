#![deny(missing_docs)]
//! This crate defines a type [`Card`] for an individual card ([`Card`]) and
//! another type [`Cards`] for a deck or hand of cards.

// pub mod simd;

use rand::Rng;
use serde::{Deserialize, Serialize};

/// A single card
#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Hash, PartialOrd, Ord)]
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
            10 => "10",
            11 => "J",
            12 => "Q",
            13 => "K",
            14 => "A",
            _ => "?",
        }
    }
    /// What is my rank called?
    pub const fn rankchar(self) -> char {
        match self.rank() {
            2 => '2',
            3 => '3',
            4 => '4',
            5 => '5',
            6 => '6',
            7 => '7',
            8 => '8',
            9 => '9',
            10 => 'T',
            11 => 'J',
            12 => 'Q',
            13 => 'K',
            14 => 'A',
            _ => '?',
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
        let shift = match self.rank() {
            14 => 1,
            13 => 14,
            12 => 13,
            n => n as u32,
        };
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

    assert_eq!('🂫', Card::SJ.unicode());
    assert_eq!('🂭', Card::SQ.unicode());
    assert_eq!('🂮', Card::SK.unicode());
}

impl std::fmt::Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.unicode())
    }
}

/// The four suits
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
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
    /// The unicode character for this suit
    pub fn unicode(self) -> char {
        match self {
            Suit::Clubs => '♣',
            Suit::Diamonds => '♦',
            Suit::Hearts => '♥',
            Suit::Spades => '♠',
        }
    }
    /// The latin character for this suit
    pub fn latin(self) -> char {
        match self {
            Suit::Clubs => 'C',
            Suit::Diamonds => 'D',
            Suit::Hearts => 'H',
            Suit::Spades => 'S',
        }
    }
    fn iter() -> impl Iterator<Item = Suit> {
        [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades]
            .iter()
            .copied()
    }
    /// All four suits from lowest to highest
    pub const ALL: &'static [Self] = &[Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades];
}

/// A deck or hand of cards
///
/// This data structure assumes that each card can appear only once, as in the
/// game bridge.  If you want to allow more than one deck mixed together you
/// want a different crate.  Because each card can only appear once, `Cards` is
/// `Copy` and only takes 64 bits.  This makes it fast and memory efficient.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize, Hash)]
pub struct Cards {
    bits: u64,
}

impl Cards {
    /// How many cards are there?
    pub const fn len(&self) -> usize {
        self.bits.count_ones() as usize
    }
    /// Is it empty?
    pub const fn is_empty(&self) -> bool {
        self.bits == 0
    }
    /// Is it non-empty?
    pub const fn non_empty(&self) -> bool {
        self.bits != 0
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

    /// Remove some cards
    pub const fn difference(&self, cards: Cards) -> Cards {
        Cards {
            bits: self.bits & !cards.bits,
        }
    }

    /// Randomly pick `num` cards to remove from the deck.
    /// Returns `None` only if there aren't enough cards.
    pub fn pick(&mut self, num: usize) -> Option<Cards> {
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

    /// All the cards in specified suit
    pub fn in_suit(self, suit: Suit) -> Cards {
        let offset = suit as i32 * 16;
        Cards {
            bits: ((self.bits >> offset) & 0xFFFF) << offset,
        }
    }

    /// All the cards divided by suits
    pub fn in_suits(self) -> PerSuit<Cards> {
        PER_SUITS.map(|suit| {
            let offset = suit as i32 * 16;
            Cards {
                bits: ((self.bits >> offset) & 0xFFFF) << offset,
            }
        })
    }
    /// All the cards in specified suit, or all the cards if the suit is void
    pub fn following_suit(self, suit: Suit) -> Cards {
        let insuit = self.in_suit(suit);
        if insuit.bits == 0 {
            self
        } else {
            insuit
        }
    }
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
    /// The tens
    pub const TENS: Cards = Cards::EMPTY
        .insert(Card::C10)
        .insert(Card::D10)
        .insert(Card::H10)
        .insert(Card::S10);
    /// Just the tens from this hand
    pub const fn tens(self) -> Cards {
        self.intersection(Cards::TENS)
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

    /// Protected high card points, meant to be used sith scp
    pub const fn protected_high_card_points(self) -> usize {
        self.aces().len() * 4
            + self.intersection(Cards::KINGS).len()
            + self
                .long_suits(2)
                .intersection(Cards::KINGS.union(Cards::QUEENS))
                .len()
            + self
                .long_suits(3)
                .intersection(Cards::KINGS.union(Cards::QUEENS).union(Cards::JACKS))
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

    /// NLTC
    pub fn new_losing_trick_count(self) -> f64 {
        let mut total = 0.0;
        for s in Suit::iter() {
            let x = self.in_suit(s);
            let len = x.len();
            if len >= 1 && x.intersection(Cards::ACES).bits == 0 {
                total += 1.5;
            }
            if len >= 2 && x.intersection(Cards::KINGS).bits == 0 {
                total += 1.0;
            }
            if len >= 3 && x.intersection(Cards::QUEENS).bits == 0 {
                total += 0.5;
            }
        }
        total
    }

    /// Return the hand valuation
    pub fn values(self) -> HandValuation {
        let suits = self.in_suits();
        let length = suits.map(|s| s.len() as u8);
        let hcp_in_suit = suits.map(|s| s.high_card_points() as u8);
        let hcp = hcp_in_suit.sum();
        let lhcp = self.long_card_points() as u8 + hcp;
        let hcp_outside_suit = hcp_in_suit.map(|i| hcp - i);
        HandValuation {
            hcp,
            shcp: self.protected_high_card_points() as u8 + self.short_card_points() as u8,
            nltc2: (self.new_losing_trick_count() * 2.0) as u8,
            lhcp,
            length,
            hcp_in_suit,
            hcp_outside_suit,
        }
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

impl std::ops::Sub for Cards {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        self.difference(rhs)
    }
}

impl std::ops::SubAssign for Cards {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.difference(rhs)
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
            self.bits &= !(1 << next);
            Some(Card { offset: next as u8 })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }

    fn count(self) -> usize {
        self.len()
    }

    fn max(self) -> Option<Card> {
        if self.bits == 0 {
            None
        } else {
            let next = self.bits.leading_zeros();
            Some(Card { offset: (63 - next) as u8 })
        }
    }

    fn min(self) -> Option<Card> {
        self.clone().next()
    }
}

impl std::fmt::Display for Cards {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for suit in [Suit::Spades, Suit::Hearts, Suit::Diamonds, Suit::Clubs]
            .iter()
            .cloned()
        {
            let cards = self.in_suit(suit);
            if !cards.is_empty() {
                write!(f, "{}", suit.unicode())?;
                for c in cards.rev() {
                    write!(f, "{}", c.rankchar())?;
                }
            }
        }
        Ok(())
    }
}

impl std::str::FromStr for Cards {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut cards = Cards::EMPTY;
        let mut suit = Suit::Spades;
        for c in s.chars() {
            match c {
                '♣' | 'C' | 'c' => {
                    suit = Suit::Clubs;
                }
                '♦' | 'D' | 'd' => {
                    suit = Suit::Diamonds;
                }
                '♥' | 'H' | 'h' => {
                    suit = Suit::Hearts;
                }
                '♠' | 'S' | 's' => {
                    suit = Suit::Spades;
                }
                '2' => {
                    cards = cards.insert(Card::new(suit, 2));
                }
                '3' => {
                    cards = cards.insert(Card::new(suit, 3));
                }
                '4' => {
                    cards = cards.insert(Card::new(suit, 4));
                }
                '5' => {
                    cards = cards.insert(Card::new(suit, 5));
                }
                '6' => {
                    cards = cards.insert(Card::new(suit, 6));
                }
                '7' => {
                    cards = cards.insert(Card::new(suit, 7));
                }
                '8' => {
                    cards = cards.insert(Card::new(suit, 8));
                }
                '9' => {
                    cards = cards.insert(Card::new(suit, 9));
                }
                'T' | '1' => {
                    cards = cards.insert(Card::new(suit, 10));
                }
                'J' => {
                    cards = cards.insert(Card::new(suit, 11));
                }
                'Q' => {
                    cards = cards.insert(Card::new(suit, 12));
                }
                'K' => {
                    cards = cards.insert(Card::new(suit, 13));
                }
                'A' => {
                    cards = cards.insert(Card::new(suit, 14));
                }
                _ => (),
            }
        }
        Ok(cards)
    }
}

#[test]
fn card_display() {
    assert_eq!("", &format!("{}", Cards::EMPTY));
    assert_eq!("".parse(), Ok(Cards::EMPTY));

    assert_eq!("♠AKQJT98765432", &format!("{}", Cards::SPADES));
    assert_eq!("♠AKQJT98765432".parse(), Ok(Cards::SPADES));

    assert_eq!(
        "♠AKQJT98765432♥AKQJT98765432♦AKQJT98765432♣AKQJT98765432",
        &format!("{}", Cards::ALL)
    );
    assert_eq!(
        "♠AKQJT98765432♥AKQJT98765432♦AKQJT98765432♣AKQJT98765432".parse(),
        Ok(Cards::ALL)
    );
    assert_eq!(
        "sAKQJT98765432 hAKQJT98765432 dAKQJT98765432 cAKQJT98765432".parse(),
        Ok(Cards::ALL)
    );
    assert_eq!(
        "s: AKQJT98765432 h: AKQJT98765432 d: AKQJT98765432 c: AKQJT98765432".parse(),
        Ok(Cards::ALL)
    );
}

impl DoubleEndedIterator for Cards {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            None
        } else {
            let next = 63 - self.bits.leading_zeros();
            self.bits &= !(1 << next);
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
    assert_eq!(cards.max(), None);
    assert_eq!(cards.min(), None);
    assert_eq!(cards.len(), 0);
    cards = cards.insert(Card::C2);
    assert_eq!(cards.min(), Some(Card::C2));
    assert_eq!(cards.max(), Some(Card::C2));
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

/// The counts associated with a given hand
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct HandValuation {
    /// The high card points
    pub hcp: u8,
    /// The high card points + long card points
    pub lhcp: u8,
    /// The protected high card points + short card points
    pub shcp: u8,
    /// Twice the New Losing Trick Count
    pub nltc2: u8,
    /// The suit length
    pub length: PerSuit<u8>,
    /// The number of HCP in the suit
    pub hcp_in_suit: PerSuit<u8>,
    /// The number of HCP outside the suit
    pub hcp_outside_suit: PerSuit<u8>,
}

impl HandValuation {
    /// The highest possible valuation
    pub const MAX: HandValuation = HandValuation {
        hcp: 37,
        lhcp: 37,
        shcp: 37,
        nltc2: 28,
        length: PerSuit { internal: [13; 4] },
        hcp_in_suit: PerSuit { internal: [10; 4] },
        hcp_outside_suit: PerSuit { internal: [30; 4] },
    };
    /// The lowest possible valuation
    pub const MIN: HandValuation = HandValuation {
        hcp: 0,
        lhcp: 0,
        shcp: 0,
        nltc2: 0,
        length: PerSuit { internal: [0; 4] },
        hcp_in_suit: PerSuit { internal: [0; 4] },
        hcp_outside_suit: PerSuit { internal: [0; 4] },
    };
    /// Am I greater than or equal on any parameter?
    #[inline]
    pub fn exceeds(self, upper: HandValuation) -> bool {
        self.hcp > upper.hcp
            || self.lhcp > upper.lhcp
            || self.shcp > upper.lhcp
            || self.nltc2 > upper.nltc2
            || self.length.exceeds(upper.length)
            || self.hcp_in_suit.exceeds(upper.hcp_in_suit)
            || self.hcp_outside_suit.exceeds(upper.hcp_outside_suit)
    }
    /// Min of both
    #[inline]
    pub fn min(self, other: HandValuation) -> HandValuation {
        use std::cmp::min;
        HandValuation {
            hcp: min(self.hcp, other.hcp),
            lhcp: min(self.lhcp, other.lhcp),
            shcp: min(self.shcp, other.shcp),
            nltc2: min(self.nltc2, other.nltc2),
            length: self.length.min_with(other.length),
            hcp_in_suit: self.hcp_in_suit.min_with(other.hcp_in_suit),
            hcp_outside_suit: self.hcp_outside_suit.min_with(other.hcp_outside_suit),
        }
    }
    /// Max of both
    #[inline]
    pub fn max(self, other: HandValuation) -> HandValuation {
        use std::cmp::max;
        HandValuation {
            hcp: max(self.hcp, other.hcp),
            lhcp: max(self.lhcp, other.lhcp),
            shcp: max(self.shcp, other.shcp),
            nltc2: max(self.nltc2, other.nltc2),
            length: self.length.max_with(other.length),
            hcp_in_suit: self.hcp_in_suit.max_with(other.hcp_in_suit),
            hcp_outside_suit: self.hcp_outside_suit.max_with(other.hcp_outside_suit),
        }
    }
    /// Modify self to have hcp
    pub fn with_hcp(mut self, hcp: u8) -> Self {
        self.hcp = hcp;
        self
    }
    /// Modify self to have shcp
    pub fn with_shcp(mut self, shcp: u8) -> Self {
        self.shcp = shcp;
        self
    }
    /// Modify self to have lhcp
    pub fn with_lhcp(mut self, lhcp: u8) -> Self {
        self.lhcp = lhcp;
        self
    }
}

impl HandValuation {}

/// A data type to hold a mapping of Cards
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CardMap {
    internal: [Card; 64],
}
impl Default for CardMap {
    /// Create an empty CardMap
    fn default() -> Self {
        CardMap {
            internal: [Card::new(Suit::Clubs, 0); 64],
        }
    }
}

impl std::ops::Index<Card> for CardMap {
    type Output = Card;

    fn index(&self, index: Card) -> &Card {
        &self.internal[index.offset as usize]
    }
}

impl std::ops::IndexMut<Card> for CardMap {
    fn index_mut(&mut self, index: Card) -> &mut Card {
        &mut self.internal[index.offset as usize]
    }
}

/// A data type to hold an array of items, one per suit.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PerSuit<T> {
    internal: [T; 4],
}

/// The suits arranged in a PerSuit data structure.
const PER_SUITS: PerSuit<Suit> = PerSuit {
    internal: [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades],
};

impl<T> PerSuit<T> {
    fn map<U, F: Fn(T) -> U>(self, f: F) -> PerSuit<U> {
        let [c, d, h, s] = self.internal;
        PerSuit {
            internal: [f(c), f(d), f(h), f(s)],
        }
    }
}

impl<T> From<[T; 4]> for PerSuit<T> {
    fn from(internal: [T; 4]) -> Self {
        PerSuit { internal }
    }
}
impl<T> PerSuit<T> {
    /// Create a PerSuit from an array
    pub const fn new(internal: [T; 4]) -> Self {
        PerSuit { internal }
    }
    /// Iterate over each suit
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.internal.iter()
    }
}

impl<T> std::ops::Index<Suit> for PerSuit<T> {
    type Output = T;

    fn index(&self, index: Suit) -> &T {
        &self.internal[index as usize]
    }
}

impl<T> std::ops::IndexMut<Suit> for PerSuit<T> {
    fn index_mut(&mut self, index: Suit) -> &mut T {
        &mut self.internal[index as usize]
    }
}

impl PerSuit<usize> {
    /// add up elements
    pub fn sum(self) -> usize {
        self.internal[0] + self.internal[1] + self.internal[2] + self.internal[3]
    }
}

impl PerSuit<u8> {
    /// add up elements
    pub fn sum(self) -> u8 {
        self.internal[0] + self.internal[1] + self.internal[2] + self.internal[3]
    }
    fn exceeds(self, upper: Self) -> bool {
        self.internal[0] > upper.internal[0]
            || self.internal[1] > upper.internal[1]
            || self.internal[2] > upper.internal[2]
            || self.internal[3] > upper.internal[3]
    }
    fn min_with(self, other: Self) -> Self {
        use std::cmp::min;
        PerSuit {
            internal: [
                min(self.internal[0], other.internal[0]),
                min(self.internal[1], other.internal[1]),
                min(self.internal[2], other.internal[2]),
                min(self.internal[3], other.internal[3]),
            ],
        }
    }
    fn max_with(self, other: Self) -> Self {
        use std::cmp::max;
        PerSuit {
            internal: [
                max(self.internal[0], other.internal[0]),
                max(self.internal[1], other.internal[1]),
                max(self.internal[2], other.internal[2]),
                max(self.internal[3], other.internal[3]),
            ],
        }
    }
}
