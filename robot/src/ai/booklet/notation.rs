//! Writing auctions as bridge players do, with those that differ only in
//! their suits or in one call written once, as in "1X – 1Y – 1NT/2X – 2NT".

use super::walk::Meanings;
use crate::Bid;
use bridge_deck::Suit;
use display_as::{format_as, FormattedString, HTML};
use std::collections::BTreeSet;
use wording::{or, Text};

/// A call of an auction with its suit left open.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Slot {
    Pass,
    Double,
    Redouble,
    /// A bid at this level, in notrump if the second number is `NOTRUMP`,
    /// and otherwise in the suit of that number among the auction's suits,
    /// counted in the order they were first bid.
    Bid(usize, usize),
}

pub(super) const NOTRUMP: usize = 4;

/// One auction of a pattern, as a number for each way that the auctions of
/// a pattern differ: the passes before the opening bid, then what each call
/// before the last means (see [`Meanings::of`]), then each of the pattern's
/// suits.
pub(super) type Filling = Vec<usize>;

/// Where a filling has the passes before the opening bid.
pub(super) const SEAT: usize = 0;

/// Where the fillings of a pattern of this many calls have what each call
/// before the last means.
pub(super) fn meant(calls: usize) -> std::ops::Range<usize> {
    1..calls
}

/// Where a filling says what the call at `call` means, which is not the
/// last call.
pub(super) fn meaning_of(call: usize) -> usize {
    call + 1
}

/// Where fillings as long as `dims`, of a pattern of this many calls, have
/// its suits.
pub(super) fn suited(calls: usize, dims: usize) -> std::ops::Range<usize> {
    calls..dims
}

/// The calls from the opening bid on with their suits left open, and the
/// suits.
pub(super) fn pattern(calls: &[Bid]) -> (Vec<Slot>, Vec<Suit>) {
    let mut suits = Vec::new();
    let slots = calls
        .iter()
        .map(|&call| match call {
            Bid::Pass => Slot::Pass,
            Bid::Double => Slot::Double,
            Bid::Redouble => Slot::Redouble,
            Bid::NT(level) => Slot::Bid(level, NOTRUMP),
            Bid::Suit(level, suit) => {
                let number = suits.iter().position(|&s| s == suit).unwrap_or_else(|| {
                    suits.push(suit);
                    suits.len() - 1
                });
                Slot::Bid(level, number)
            }
        })
        .collect();
    (slots, suits)
}

/// The seats of players who call after this many passes, as in "first or
/// second seat".
pub(super) fn seats(passes: impl IntoIterator<Item = usize>) -> Text {
    pub(super) const SEATS: [&str; 4] = ["first", "second", "third", "fourth"];
    or(passes.into_iter().map(|p| SEATS[p])) + "seat"
}

/// One of the calls that a call of a [`Line`] may be.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Choice {
    pub(super) slot: Slot,
    /// The call as it is written, as in "2Z" or "(Dbl)".
    pub(super) call: String,
    /// What the call means, when that needs saying.
    pub(super) note: Option<String>,
    /// Whether the call is written with a few suits, like 2♦/♥.
    pub(super) several: bool,
}

/// Some auctions as bridge players write them, as in "1X – (Dbl) – 1Y",
/// where the last call is the one being described.  The calls of that
/// player's opponents are in parentheses, and are left out when they are
/// all passes.
#[derive(Clone)]
pub(super) struct Line {
    /// The calls that are written, each with every call it may be, as in
    /// "1X – 1Y – 1NT/2X – 2NT".
    pub(super) calls: Vec<Vec<Choice>>,
    /// The seats that the opening bid may be in, when that needs saying.
    pub(super) seats: Option<String>,
    /// More that the line is true of, as in "or over (Dbl)".
    pub(super) also: String,
    /// The length and the first of the auctions, to put lines in order.
    pub(super) first: (usize, Vec<Slot>, Vec<usize>),
}

impl Line {
    /// How many notes follow the auction.
    pub(super) fn notes(&self) -> usize {
        let noted = self.calls.iter().flatten();
        noted.filter(|choice| choice.note.is_some()).count() + self.seats.iter().count()
    }
    /// This line and `other` as one, when all that differs is what one of
    /// their calls may be.
    pub(super) fn with(&self, other: &Line) -> Option<Line> {
        if self.calls.len() != other.calls.len()
            || self.seats != other.seats
            || self.also != other.also
        {
            return None;
        }
        let mut differing = (0..self.calls.len()).filter(|&i| self.calls[i] != other.calls[i]);
        let (Some(i), None) = (differing.next(), differing.next()) else {
            return None;
        };
        let mut choices = [&self.calls[i][..], &other.calls[i][..]].concat();
        choices.sort();
        choices.dedup();
        // "2♦/♥/2NT" would be hard to read, and a call with a note cannot
        // also be that call whatever it means.
        let unclear = |a: &Choice| {
            let half_noted = |b: &Choice| a.call == b.call && a.note.is_some() != b.note.is_some();
            a.several || choices.iter().any(half_noted)
        };
        if choices.iter().any(unclear) {
            return None;
        }
        let mut line = self.clone();
        line.calls[i] = choices;
        line.first = self.first.clone().min(other.first.clone());
        Some(line)
    }
    /// The line as HTML, with its notes after the auction.
    pub(super) fn html(&self) -> FormattedString<HTML> {
        let mut calls = Vec::new();
        let mut notes = Vec::new();
        for choices in &self.calls {
            let mut written: Vec<&str> = Vec::new();
            for choice in choices.iter().filter(|choice| choice.note.is_some()) {
                if !written.contains(&choice.call.as_str()) {
                    let same = choices.iter().filter(|other| other.call == choice.call);
                    let meanings: Vec<&str> =
                        same.flat_map(|other| other.note.as_deref()).collect();
                    notes.push(format!("{}: {}", choice.call, meanings.join(" or ")));
                }
                written.push(&choice.call);
            }
            let mut written: Vec<&str> = choices.iter().map(|choice| &choice.call[..]).collect();
            written.dedup();
            calls.push(written.join("/"));
        }
        if let Some(last) = calls.last_mut() {
            *last = format!("<strong>{last}</strong>");
        }
        notes.extend(self.seats.clone());
        let mut html = calls.join(" – ");
        if !notes.is_empty() {
            html += &format!(" <span class=\"note\">({})</span>", notes.join("; "));
        }
        if !self.also.is_empty() {
            html += &format!(" <span class=\"note\">{}</span>", self.also);
        }
        FormattedString::from_formatted(html)
    }
}

/// `lines` with those that differ in one call written as one line, in the
/// order of their first auctions.
pub(super) fn merged(mut lines: Vec<Line>) -> Vec<Line> {
    loop {
        let pairs = (0..lines.len()).flat_map(|i| (0..i).map(move |j| (j, i)));
        let Some((j, i, both)) = pairs
            .filter_map(|(j, i)| lines[j].with(&lines[i]).map(|both| (j, i, both)))
            .next()
        else {
            break;
        };
        lines[j] = both;
        lines.remove(i);
    }
    lines.sort_by(|a, b| a.first.cmp(&b.first));
    lines
}

/// Some auctions of one pattern as a line.
///
/// `values` are the auctions' fillings, one set for each number of a
/// filling, and `open` says which of those sets need not be written, because
/// the pattern has no other auctions.  Seats and meanings that do need
/// writing become notes, and a suit is written as itself, as a letter for
/// any suit, as M for a major or m for a minor, or as a few suits, like
/// ♦/♥/♠.  This is nothing when the suits need more than that, or a note
/// would have to list several meanings of one call, either of which would
/// be hard to read.
pub(super) fn write(
    slots: &[Slot],
    values: &[BTreeSet<usize>],
    open: &[bool],
    all: &[Filling],
    meanings: &Meanings,
) -> Option<Line> {
    let last = slots.len() - 1;
    let suit = |v: &usize| format_as!(HTML, Suit::ALL[*v]).into_string();
    // How many suits are written as M, as m, and as several suits.
    let (mut majors, mut minors, mut several) = (0, 0, 0);
    let mut strains = Vec::new();
    for (number, d) in suited(slots.len(), values.len()).enumerate() {
        let suits: Vec<usize> = values[d].iter().copied().collect();
        strains.push(match suits[..] {
            [only] => (suit(&only), false),
            _ if open[d] => (["X", "Y", "Z", "W"][number].to_string(), false),
            [2, 3] => {
                majors += 1;
                ("M".to_string(), false)
            }
            [0, 1] => {
                minors += 1;
                ("m".to_string(), false)
            }
            _ => {
                several += 1;
                (suits.iter().map(suit).collect::<Vec<_>>().join("/"), true)
            }
        });
    }
    if majors > 1 || minors > 1 || several > 1 || (several == 1 && majors + minors > 0) {
        return None;
    }
    // A letter that can only be a major or a minor says so, when no other
    // suit of the line is written that way.
    for (number, d) in suited(slots.len(), values.len()).enumerate() {
        let suits: Vec<usize> = values[d].iter().copied().collect();
        if open[d] && several == 0 {
            if suits == [2, 3] && majors == 0 {
                majors += 1;
                strains[number].0 = "M".to_string();
            } else if suits == [0, 1] && minors == 0 {
                minors += 1;
                strains[number].0 = "m".to_string();
            }
        }
    }
    let theirs = |i: usize| (last - i) % 2 == 1;
    let contested = (0..last).any(|i| theirs(i) && slots[i] != Slot::Pass);
    let mut calls = Vec::new();
    for i in (0..=last).filter(|&i| contested || !theirs(i)) {
        let (call, several) = match slots[i] {
            Slot::Pass => ("Pass".to_string(), false),
            Slot::Double => ("Dbl".to_string(), false),
            Slot::Redouble => ("Rdbl".to_string(), false),
            Slot::Bid(level, NOTRUMP) => (format!("{level}NT"), false),
            Slot::Bid(level, number) => {
                (format!("{level}{}", strains[number].0), strains[number].1)
            }
        };
        let mut note = None;
        if i < last && !open[meaning_of(i)] {
            let mut meant = values[meaning_of(i)].iter();
            let (Some(&meaning), None) = (meant.next(), meant.next()) else {
                return None;
            };
            let others: BTreeSet<usize> =
                all.iter().map(|filling| filling[meaning_of(i)]).collect();
            note = Some(meanings.name(meaning, &others));
        }
        calls.push(vec![Choice {
            slot: slots[i],
            call: if theirs(i) { format!("({call})") } else { call },
            note,
            several,
        }]);
    }
    let seats = (!open[SEAT]).then(|| {
        let when = if last == 0 { "in" } else { "opened in" };
        let seats = Text::from(when) + seats(values[SEAT].iter().copied());
        format_as!(HTML, seats).into_string()
    });
    let suits = |d: usize| *values[d].first().expect("a suit of no auction");
    Some(Line {
        calls,
        seats,
        also: String::new(),
        first: (
            slots.len(),
            slots.to_vec(),
            suited(slots.len(), values.len()).map(suits).collect(),
        ),
    })
}

/// The fewest ways found of writing the auctions `mine` of one pattern,
/// which are some of the auctions `all` of that pattern that a player might
/// look up.  Nothing is written that the other auctions make plain: a suit
/// is a letter when the auctions with every other suit there are meant too.
pub(super) fn written(
    slots: &[Slot],
    mine: &[Filling],
    all: &[Filling],
    meanings: &Meanings,
) -> Vec<Line> {
    let calls = slots.len();
    let dims = mine[0].len();
    let values: Vec<BTreeSet<usize>> = (0..dims)
        .map(|d| mine.iter().map(|filling| filling[d]).collect())
        .collect();
    // How many of all the auctions are like mine in every way that is not
    // `open`.
    let like = |open: &[bool]| {
        let like =
            |filling: &&Filling| (0..dims).all(|d| open[d] || values[d].contains(&filling[d]));
        all.iter().filter(like).count()
    };
    let mut best: Option<Line> = None;
    if like(&vec![false; dims]) == mine.len() {
        // One line says it.  What a call means can stand for its suit, as
        // in a reverse, or the suit for what the call means, so try leaving
        // out each first, and keep the line with the fewest notes.
        let suits_first = suited(calls, dims).chain(meant(calls)).chain([SEAT]);
        let meanings_first = meant(calls).chain(suited(calls, dims)).chain([SEAT]);
        for order in [meanings_first.collect::<Vec<_>>(), suits_first.collect()] {
            let mut open = vec![false; dims];
            // A single suit is as short as a letter, and says more.
            for d in order
                .into_iter()
                .filter(|&d| !suited(calls, dims).contains(&d) || values[d].len() > 1)
            {
                open[d] = true;
                open[d] = like(&open) == mine.len();
            }
            if let Some(line) = write(slots, &values, &open, all, meanings) {
                if best.as_ref().is_none_or(|best| line.notes() < best.notes()) {
                    best = Some(line);
                }
            }
        }
    }
    if let Some(line) = best {
        return vec![line];
    }
    // Otherwise write apart the auctions for each meaning of a call, each
    // choice of a suit or each seat, whichever takes the fewest lines.
    let several = |mut ds: std::ops::Range<usize>| ds.find(|&d| values[d].len() > 1);
    let apart = [
        several(meant(calls)),
        several(suited(calls, dims)),
        several(SEAT..SEAT + 1),
    ];
    let lines = apart.into_iter().flatten().map(|d| {
        let lines = values[d].iter().flat_map(|&value| {
            let of = |fillings: &[Filling]| -> Vec<Filling> {
                let with_value = fillings.iter().filter(|filling| filling[d] == value);
                with_value.cloned().collect()
            };
            written(slots, &of(mine), &of(all), meanings)
        });
        lines.collect::<Vec<Line>>()
    });
    lines
        .min_by_key(|lines| lines.len())
        .expect("auctions of one pattern that differ in nothing")
}
