//! An order of plays fitted to double-dummy results, in place of the tiers of
//! [`likely_plays`](super::likely_plays): an n-tuple network, which scores a
//! play by adding up one entry of each of a few tables, each looked up by a
//! few plain facts about the play, such as who holds the cards just above
//! it.  The entries are fitted by the ignored tests in `ordering.rs`; nothing
//! about which play is good is written here, only which facts each table
//! sees.

use super::Table;
use bridge_deck::{Card, Cards, Suit};

/// The tables, each with its size and the facts that index it.  The seat is
/// the next player's place in the trick, from 0 for the leader, and how a
/// card is played is whether it leads, follows suit, ruffs or is discarded.
pub(crate) const TUPLES: [(usize, &str); 6] = [
    (
        4 * 4 * 2 * 2 * 2,
        "the seat, how the card is played, whether partner is winning the trick, \
         and whether the card beats the trick so far and holds",
    ),
    (4 * 125, "the seat, and who holds each of the three cards just above"),
    (4 * 125, "the seat, and who holds each of the three cards just below"),
    (
        4 * 4 * 27,
        "the seat, the player's length in the suit, and whether each other \
         player has the suit, can ruff it, or has neither",
    ),
    (
        4 * 4 * 6 * 2,
        "the seat, how the card is played, how many cards of its suit are left \
         above it, and whether it is the player's lowest of the suit",
    ),
    (4 * 4 * 6, "the seat, how the card is played, and how many tricks are left"),
];

/// How many entries the tables have, end to end.
#[cfg(test)]
pub(crate) const ENTRIES: usize = {
    let mut entries = 0;
    let mut i = 0;
    while i < TUPLES.len() {
        entries += TUPLES[i].0;
        i += 1;
    }
    entries
};

/// What the plays of one position have in common: the next player to a
/// trick, from hands renumbered among the cards left, as the solver keeps
/// them.
pub(crate) struct Position<'a> {
    table: Table<'a>,
    seat: usize,
    /// The hands from the next player's: theirs, the next player's after
    /// them, their partner's, and the one before them.
    relative: [Cards; 4],
    lead: Option<Suit>,
    winning: Option<Card>,
    partner_winning: bool,
}

impl<'a> Position<'a> {
    /// The next player to `trick`, from `hands` as they were before it, in
    /// the order of play from its leader.
    pub(crate) fn new(trump: Option<Suit>, hands: &'a [Cards; 4], trick: &'a [Card]) -> Self {
        let table = Table {
            trump,
            hands,
            trick,
            all: hands[0] + hands[1] + hands[2] + hands[3],
        };
        let seat = trick.len();
        let winner = (seat > 0).then(|| table.winner_so_far());
        Position {
            seat,
            relative: [0, 1, 2, 3].map(|i| hands[(seat + i) % 4]),
            lead: trick.first().map(|card| card.suit()),
            winning: winner.map(|w| trick[w]),
            partner_winning: winner.is_some_and(|w| w & 1 == seat & 1),
            table,
        }
    }

    /// The plays worth trying, the same ones that
    /// [`likely_plays`](super::likely_plays) gives: of cards that touch, only
    /// the lowest.
    pub(crate) fn plays(&self) -> Cards {
        let hand = self.relative[0];
        let legal = match self.lead {
            None => hand,
            Some(lead) => hand.following_suit(lead),
        };
        legal.lowest_of_sequences(self.table.all)
    }

    /// The entries of the tables of [`TUPLES`], laid end to end, that playing
    /// `card` looks up, one in each table.
    pub(crate) fn entries(&self, card: Card) -> [usize; TUPLES.len()] {
        let suit = card.suit();
        let seat = self.seat;
        let trump = self.table.trump;
        let how = match self.lead {
            None => 0,
            Some(lead) if lead == suit => 1,
            Some(_) if trump == Some(suit) => 2,
            Some(_) => 3,
        };
        let beats = self
            .winning
            .is_some_and(|winning| self.table.beats(card, winning));
        let holds = beats && self.table.holds(card);
        let mine = self.relative[0].in_suit(suit);
        let length = mine.len().min(4) - 1;
        let others = [1, 2, 3].map(|i| {
            let hand = self.relative[i];
            if !hand.in_suit(suit).is_empty() {
                0
            } else if trump.is_some_and(|t| t != suit && !hand.in_suit(t).is_empty()) {
                1
            } else {
                2
            }
        });
        let above = (self.table.all.in_suit(suit) & Cards::higher_than(card))
            .len()
            .min(5);
        let lowest = mine.min() == Some(card);
        let tricks = match self.table.hands[0].len() {
            0..=1 => 0,
            2 => 1,
            3 => 2,
            4..=5 => 3,
            6..=8 => 4,
            _ => 5,
        };
        let indices = [
            (((seat * 4 + how) * 2 + self.partner_winning as usize) * 2 + beats as usize) * 2
                + holds as usize,
            seat * 125 + Cards::holders_above(&self.relative, card, 3),
            seat * 125 + Cards::holders_below(&self.relative, card, 3),
            (seat * 4 + length) * 27 + others[0] * 9 + others[1] * 3 + others[2],
            ((seat * 4 + how) * 6 + above) * 2 + lowest as usize,
            (seat * 4 + how) * 6 + tricks,
        ];
        let mut entries = [0; TUPLES.len()];
        let mut offset = 0;
        for (t, &index) in indices.iter().enumerate() {
            debug_assert!(index < TUPLES[t].0, "{}", TUPLES[t].1);
            entries[t] = offset + index;
            offset += TUPLES[t].0;
        }
        entries
    }

    /// The score of playing `card`, by the network with `weights`.
    pub(crate) fn score(&self, weights: &[f32], card: Card) -> f32 {
        self.entries(card).iter().map(|&e| weights[e]).sum()
    }
}

/// The plays of a position, highest scored first.
pub(crate) struct Ordered {
    cards: [Card; 13],
    len: usize,
    next: usize,
}

impl Iterator for Ordered {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        (self.next < self.len).then(|| {
            self.next += 1;
            self.cards[self.next - 1]
        })
    }
}

/// The plays that [`likely_plays`](super::likely_plays) would give for the
/// next player to `trick`, from `hands`, highest scored by the network with
/// `weights` first.
pub(crate) fn fitted_plays(
    trump: Option<Suit>,
    hands: &[Cards; 4],
    trick: &[Card],
    weights: &[f32],
) -> Ordered {
    let position = Position::new(trump, hands, trick);
    let mut scored = [(0.0f32, Card::S2); 13];
    let mut len = 0;
    for card in position.plays() {
        let score = position.score(weights, card);
        // Insertion sort, highest first: there are at most 13 plays.
        let mut i = len;
        while i > 0 && scored[i - 1].0 < score {
            scored[i] = scored[i - 1];
            i -= 1;
        }
        scored[i] = (score, card);
        len += 1;
    }
    Ordered {
        cards: scored.map(|(_, card)| card),
        len,
        next: 0,
    }
}

/// One of two kinds of iterator.
pub(crate) enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L: Iterator<Item = Card>, R: Iterator<Item = Card>> Iterator for Either<L, R> {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        match self {
            Either::Left(l) => l.next(),
            Either::Right(r) => r.next(),
        }
    }
}

/// The plays to try for the next player to `trick`, from `hands`: in the
/// order of the network with `weights` when there are some, and otherwise
/// in that of [`likely_plays`](super::likely_plays).
pub(crate) fn plays(
    trump: Option<Suit>,
    weights: Option<&[f32]>,
    hands: &[Cards; 4],
    trick: &[Card],
) -> Either<impl Iterator<Item = Card>, Ordered> {
    match weights {
        None => Either::Left(super::likely_plays(trump, hands, trick)),
        Some(weights) => Either::Right(fitted_plays(trump, hands, trick, weights)),
    }
}
