//! The pages: a table for each stage of the auction, with a line for the
//! calls that mean the same and show the same hand.

use super::notation::{merged, pattern, written, Filling, Line, Slot, SEAT};
use super::walk::{followed_by, from_opening, rank, Meanings, Node, Stage};
use crate::ai::advice::Feature;
use crate::ai::{legal_bids, Forcing};
use crate::Bid;
use bridge_deck::Suit;
use display_as::{format_as, FormattedString, HTML};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use wording::{and, Text};

/// Other words for the suits of an auction, for the player who makes its
/// last call, as in "partner's suit".  A suit belongs to the side that bid
/// it first.
pub(super) fn roles(calls: &[Bid]) -> HashMap<Suit, &'static str> {
    pub(super) const YOURS: [&str; 3] = ["your suit", "your first suit", "your second suit"];
    pub(super) const PARTNERS: [&str; 3] = [
        "partner's suit",
        "partner's first suit",
        "partner's second suit",
    ];
    pub(super) const THEIRS: [&str; 3] = ["their suit", "their first suit", "their second suit"];
    let last = calls.len() - 1;
    let mut bidders: [Vec<Suit>; 3] = Default::default();
    for (i, call) in calls[..last].iter().enumerate() {
        if let Bid::Suit(_, suit) = *call {
            if !bidders.iter().any(|suits| suits.contains(&suit)) {
                bidders[[0, 2, 1, 2][(last - i) % 4]].push(suit);
            }
        }
    }
    let mut roles = HashMap::new();
    for (suits, words) in bidders.iter().zip([YOURS, PARTNERS, THEIRS]) {
        match suits[..] {
            [suit] => {
                roles.insert(suit, words[0]);
            }
            [first, second, ..] => {
                roles.insert(first, words[1]);
                roles.insert(second, words[2]);
            }
            [] => {}
        }
    }
    if let Bid::Suit(_, suit) = calls[last] {
        roles.insert(suit, "the suit bid");
    }
    roles
}

/// The hands that `features` ask for, one text for each way of fitting,
/// where `called` has other words for some suits.
pub(super) fn hands(
    features: &[Vec<Feature>],
    called: &dyn Fn(Suit) -> Option<&'static str>,
) -> Vec<Text> {
    features
        .iter()
        .map(|fit| and(fit.iter().map(|feature| feature.text_calling(called))))
        .collect()
}

/// A call that an entry describes, in its auction.
pub(super) struct Call {
    pub(super) entry: usize,
    /// The calls from the opening bid on, ending with this one.
    pub(super) calls: Vec<Bid>,
    /// The same as a pattern and the way this auction fills it.
    pub(super) slots: Vec<Slot>,
    pub(super) filling: Filling,
}

impl Call {
    /// The whole auction, with the passes before the opening bid, which
    /// matter to what a call means.
    pub(super) fn bids(&self) -> Vec<Bid> {
        [&vec![Bid::Pass; self.filling[SEAT]][..], &self.calls].concat()
    }
}

/// `slots` with a pass for each double and redouble by the opponents of the
/// player who makes the last call.
pub(super) fn undoubled(slots: &[Slot]) -> Vec<Slot> {
    let last = slots.len() - 1;
    let quiet = |(i, &slot): (usize, &Slot)| match slot {
        Slot::Double | Slot::Redouble if (last - i) % 2 == 1 => Slot::Pass,
        _ => slot,
    };
    slots.iter().enumerate().map(quiet).collect()
}

/// The calls that mean the same, with the hand they show.
pub(super) struct Row {
    /// The auctions, each ending in the call.
    pub(super) auctions: Vec<FormattedString<HTML>>,
    pub(super) name: &'static str,
    /// What the entries say about themselves, when they all say the same
    /// and it is more than the name.
    pub(super) description: Option<FormattedString<HTML>>,
    pub(super) forcing: Forcing,
    /// The hands that fit, one text for each way of fitting, or none when
    /// any hand fits.
    pub(super) hands: Vec<Text>,
}

impl Row {
    /// The line for `calls`, which mean the same, where `all` are the
    /// auctions of each pattern that a player might look up on this page.
    pub(super) fn new(
        meanings: &Meanings,
        calls: &[&Call],
        all: &HashMap<Vec<Slot>, Vec<Filling>>,
    ) -> Self {
        let meaning = meanings.entry(calls[0].entry);
        // A suit goes by its part in the auction only when that makes this
        // line say more than one auction can.
        let mut suits: HashMap<&str, HashSet<Suit>> = HashMap::new();
        for call in calls {
            for (suit, role) in roles(&call.calls) {
                suits.entry(role).or_default().insert(suit);
            }
        }
        let roles = roles(&calls[0].calls);
        let called = |suit| {
            let role = roles.get(&suit).copied();
            role.filter(|role| suits[role].len() > 1)
        };
        let mut patterns: BTreeMap<&[Slot], Vec<Filling>> = BTreeMap::new();
        for call in calls {
            let fillings = patterns.entry(&call.slots).or_default();
            fillings.push(call.filling.clone());
        }
        for fillings in patterns.values_mut() {
            fillings.sort();
            fillings.dedup();
        }
        // Whether the auctions of the pattern `slots`, which has doubles by
        // the opponents, are those of `quiet`, the same pattern without the
        // doubles, in the seats where the opponents have a double.
        let is_twin = |slots: &[Slot], quiet: &[Slot]| {
            let (Some(mine), Some(theirs)) = (patterns.get(slots), patterns.get(quiet)) else {
                return false;
            };
            let seats: BTreeSet<usize> = all[slots].iter().map(|filling| filling[SEAT]).collect();
            let seated = |fillings: &[Filling]| -> Vec<Filling> {
                let seated = fillings
                    .iter()
                    .filter(|filling| seats.contains(&filling[SEAT]));
                seated.cloned().collect()
            };
            slots != quiet && seated(theirs) == *mine && seated(&all[quiet]) == all[slots]
        };
        // A call that means the same after a double is written once, with
        // a note.
        let mut doubled: HashMap<Vec<Slot>, Vec<&str>> = HashMap::new();
        for &slots in patterns.keys() {
            let quiet = undoubled(slots);
            if is_twin(slots, &quiet) {
                let before = slots.len() - 2;
                let also = match slots[before] {
                    Slot::Double if slots[..before] == quiet[..before] => "or over (Dbl)",
                    Slot::Redouble if slots[..before] == quiet[..before] => "or over (Rdbl)",
                    _ => "or after their double",
                };
                // "After" says "over" too.
                let notes = doubled.entry(quiet).or_default();
                if also.contains("after") {
                    notes.clear();
                }
                if notes.is_empty() || !notes[0].contains("after") && !notes.contains(&also) {
                    notes.push(also);
                }
            }
        }
        let mut lines = Vec::new();
        for (&slots, mine) in &patterns {
            if is_twin(slots, &undoubled(slots)) {
                continue;
            }
            let also = doubled
                .get(slots)
                .map_or(String::new(), |also| also.join(" "));
            for mut line in written(slots, mine, &all[slots], meanings) {
                line.also = also.clone();
                lines.push(line);
            }
        }
        let auctions = merged(lines).iter().map(Line::html).collect();
        let said = &meaning.said;
        let all_say = |said: &String| {
            calls
                .iter()
                .all(|call| meanings.entry(call.entry).said == *said)
        };
        let description = (!said.is_empty() && said != meaning.name && all_say(said))
            .then(|| FormattedString::from_formatted(said.clone()));
        let features = meaning.features(meanings.table, &calls[0].bids());
        Row {
            auctions,
            name: meaning.name,
            description,
            forcing: meaning.forcing,
            hands: match meaning.is_never_bid() {
                true => vec![Text::from("no hand: this is never bid")],
                false => hands(&features, &called),
            },
        }
    }
}

impl Row {
    /// Whether this line has the same meaning and hands as `other`.
    pub(super) fn says_the_same_as(&self, other: &Row) -> bool {
        let hands = |row: &Row| -> Vec<String> {
            let hands = row.hands.iter();
            hands
                .map(|hand| format_as!(HTML, hand).into_string())
                .collect()
        };
        self.name == other.name
            && self.description == other.description
            && self.forcing == other.forcing
            && hands(self) == hands(other)
    }
}

/// A stage of the auction, with every call the table describes there.
pub(super) struct Page {
    pub(super) id: String,
    pub(super) title: &'static str,
    pub(super) rows: Vec<Row>,
}

impl Page {
    /// The page for `stage`, whose auctions are `nodes`.
    pub(super) fn new(meanings: &Meanings, stage: Stage, id: String, nodes: &[&Node]) -> Self {
        // Every call a player might look up here, described or not.
        let mut all: HashMap<Vec<Slot>, Vec<Filling>> = HashMap::new();
        let mut calls = Vec::new();
        for node in nodes {
            let opened = from_opening(&node.history);
            let passes = node.history.len() - opened.len();
            let earlier = meanings.of(&node.history);
            let call = |bid: Bid| {
                let calls = followed_by(opened, bid);
                let (slots, suits) = pattern(&calls);
                let suits = suits.into_iter().map(|suit| suit as usize);
                let filling = [passes].into_iter().chain(earlier.iter().copied());
                (calls, slots, filling.chain(suits).collect::<Filling>())
            };
            for bid in legal_bids(&node.history) {
                let (_, slots, filling) = call(bid);
                all.entry(slots).or_default().push(filling);
            }
            calls.extend(node.described.iter().map(|&(entry, bid)| {
                let (calls, slots, filling) = call(bid);
                Call {
                    entry,
                    calls,
                    slots,
                    filling,
                }
            }));
        }
        for fillings in all.values_mut() {
            fillings.sort();
        }
        // Calls mean the same when their entries have one name and ask for
        // the same hand, once each suit goes by its part in the auction.
        let mut alike: HashMap<String, Vec<&Call>> = HashMap::new();
        for call in &calls {
            let entry = meanings.entry(call.entry);
            let roles = roles(&call.calls);
            let features = entry.features(meanings.table, &call.bids());
            let hands = hands(&features, &|suit| roles.get(&suit).copied());
            let hands: Vec<String> = hands
                .iter()
                .map(|hand| format_as!(HTML, hand).into_string())
                .collect();
            let meaning = format!("{} {:?} {}", entry.name, entry.forcing, hands.join(" or "));
            alike.entry(meaning).or_default().push(call);
        }
        // The lines go by their auctions, lowest first, and those for one
        // call in the order of the table.
        let mut rows: Vec<_> = alike
            .into_values()
            .map(|calls| {
                let place = |call: &&Call| {
                    let ranks: Vec<usize> = call.calls.iter().map(|&bid| rank(bid)).collect();
                    (call.slots.len(), call.slots.clone(), ranks, call.entry)
                };
                let first = calls.iter().map(place).min();
                (first, Row::new(meanings, &calls, &all))
            })
            .collect();
        rows.sort_by(|a, b| a.0.cmp(&b.0));
        // Lines that come to say the same, because a suit that had a part
        // in the auction is called by its name after all, are one line.
        let mut merged: Vec<Row> = Vec::new();
        for (_, row) in rows {
            match merged.iter_mut().find(|other| other.says_the_same_as(&row)) {
                Some(other) => other.auctions.extend(row.auctions),
                None => merged.push(row),
            }
        }
        Page {
            id,
            title: stage.title().0,
            rows: merged,
        }
    }
}
