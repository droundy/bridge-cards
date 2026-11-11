//! A booklet on a bidding system, written from its table alone: what every
//! call the table describes shows, a page for each stage of the auction, such
//! as the responses to an opening of one of a suit.  It is for learning a
//! system from its opening bids on, and for looking up an auction, such as
//! the responses to a strong 2♣.
//!
//! Nothing is simulated.  The booklet follows the table from the first call:
//! each call that an entry describes leads to the auctions after it, and a
//! player the table has nothing for passes.  A page would be far too long
//! with a line for each call of each auction, so calls that mean the same
//! share a line, and auctions that differ only in their suits are written as
//! one, as in "1X – 1Y – 2Y".

use super::advice::{describe, Feature};
use super::rules::{contract_points, Kind, Points, Rule};
use super::{
    bids_string, final_bid, legal_bids, BiddingSystem, Convention, Fit, Forcing, ShapeRule,
};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use display_as::{format_as, with_template, DisplayAs, FormattedString, HTML};
use regex::RegexSet;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use wording::{and, at_least, or, Text, Word};

/// Where a call stands among all calls, lowest first.
fn rank(bid: Bid) -> usize {
    match bid {
        Bid::Pass => 0,
        Bid::Double => 1,
        Bid::Redouble => 2,
        Bid::Suit(level, suit) => 5 * level + suit as usize,
        Bid::NT(level) => 5 * level + 4,
    }
}

/// Is the auction over?
fn is_over(history: &[Bid]) -> bool {
    let n = history.len();
    n >= 4 && history[n - 3..].iter().all(|&b| b == Bid::Pass)
}

/// `history` followed by `call`.
fn followed_by(history: &[Bid], call: Bid) -> Vec<Bid> {
    [history, &[call]].concat()
}

/// `history` from its first call that is not a pass.
fn from_opening(history: &[Bid]) -> &[Bid] {
    let passes = history.iter().take_while(|&&b| b == Bid::Pass).count();
    &history[passes..]
}

/// An invitation that the contract rules make of a bid, written as an entry
/// of a table would be, or nothing for any other `meaning`.  It says of
/// itself what it invites and what that needs.
fn as_an_entry(meaning: &Convention) -> Option<Convention> {
    let Convention::Contracted {
        the_name,
        min,
        max,
        reason,
        ..
    } = meaning
    else {
        return None;
    };
    if reason.rule != Rule::Invite {
        return None;
    }
    let points = match reason.points {
        Points::Hcp => "high card points",
        Points::Lhcp => "long high card points",
        Points::Shcp => "short high card points",
    };
    let kind = Kind::of(reason.contract).name();
    let said = format!(
        "Invites a {kind}, which needs {} {points} between you",
        reason.needs
    );
    Some(Convention::Simple {
        regex: RegexSet::empty(),
        the_description: FormattedString::from_formatted(said),
        the_name,
        min: *min,
        max: *max,
        forcing: Forcing::Passable,
    })
}

/// What says outright what a call after `history` shows, each with its
/// call: the entries of `table` by their place in it, in its order, and then
/// each invitation that its contract rules make of a response to an opening
/// bid that no entry describes, which goes on the end of `ruled` and is
/// numbered from the end of the table.  That leaves out the `Natural` and
/// `Forced` entries, which are defaults for any auction, the contracts that
/// the rules place, and their invitations later in the auction, which would
/// fill the later pages with what the rules say once.
fn described(
    table: &Convention,
    history: &[Bid],
    ruled: &mut Vec<Convention>,
) -> Vec<(usize, Bid)> {
    let entries = table.entries();
    let mut bids = followed_by(history, Bid::Pass);
    let mut found = Vec::new();
    for bid in legal_bids(history) {
        *bids.last_mut().unwrap() = bid;
        let auction = bids_string(&bids);
        let before = found.len();
        for i in table.candidates(&auction) {
            let entry = &entries[i];
            if matches!(entry, Convention::Simple { .. } | Convention::Rule { .. })
                && entry.matches(&auction, &bids)
            {
                found.push((i, bid));
            }
        }
        if found.len() > before || from_opening(history).len() != 2 {
            continue;
        }
        if let Some(entry) = table.refine(&bids).as_ref().and_then(as_an_entry) {
            found.push((entries.len() + ruled.len(), bid));
            ruled.push(entry);
        }
    }
    found.sort_by_key(|&(i, _)| i);
    found
}

/// An auction that the start of the auction leads to by calls the table
/// describes, where a player the table has nothing for passes once.
struct Node {
    history: Vec<Bid>,
    /// What describes the next call, each with its call: see [`described`].
    described: Vec<(usize, Bid)>,
}

/// Every such auction, and the invitations in them that the contract rules
/// make of bids no entry describes.
fn walk(table: &Convention) -> (Vec<Node>, Vec<Convention>) {
    let mut nodes = Vec::new();
    let mut ruled = Vec::new();
    let mut seen = HashSet::new();
    // Each auction to look at, and whether its last call is a pass that no
    // entry describes.
    let mut todo = vec![(Vec::new(), false)];
    while let Some((history, assumed)) = todo.pop() {
        if is_over(&history) || !seen.insert(bids_string(&history)) {
            continue;
        }
        let described = described(table, &history, &mut ruled);
        // An invitation is answered by the contract rules, like any natural
        // bid, so the auctions after one are not followed.
        let in_table = |&&(i, _): &&(usize, Bid)| i < table.entries().len();
        let mut calls: Vec<Bid> = described
            .iter()
            .filter(in_table)
            .map(|&(_, bid)| bid)
            .collect();
        calls.sort_by_key(|&bid| rank(bid));
        calls.dedup();
        if !assumed && !calls.contains(&Bid::Pass) {
            todo.push((followed_by(&history, Bid::Pass), true));
        }
        for call in calls {
            todo.push((followed_by(&history, call), false));
        }
        nodes.push(Node { history, described });
    }
    (nodes, ruled)
}

/// The opening bids, in groups that share their pages: the bids of one level
/// that the table gives the same name, such as the weak twos.
struct Openings(Vec<Vec<Bid>>);

impl Openings {
    fn new(meanings: &Meanings, nodes: &[Node]) -> Self {
        let mut openers: Vec<&Node> = nodes
            .iter()
            .filter(|node| from_opening(&node.history).is_empty())
            .collect();
        openers.sort_by_key(|node| node.history.len());
        // Each bid goes by its name in the first seat that has one.
        let mut groups: Vec<((usize, &str), Vec<Bid>)> = Vec::new();
        for &(i, bid) in openers.iter().flat_map(|node| &node.described) {
            let (Bid::Suit(level, _) | Bid::NT(level)) = bid else {
                continue;
            };
            if groups.iter().any(|(_, bids)| bids.contains(&bid)) {
                continue;
            }
            let key = (level, meanings.entry(i).the_name());
            match groups.iter_mut().find(|(k, _)| *k == key) {
                Some((_, bids)) => bids.push(bid),
                None => groups.push((key, vec![bid])),
            }
        }
        let mut groups: Vec<Vec<Bid>> = groups.into_iter().map(|(_, bids)| bids).collect();
        for bids in groups.iter_mut() {
            bids.sort_by_key(|&bid| rank(bid));
        }
        groups.sort_by_key(|bids| rank(bids[0]));
        Openings(groups)
    }
    /// The group that an opening bid is in.
    fn group(&self, bid: Bid) -> usize {
        self.0
            .iter()
            .position(|bids| bids.contains(&bid))
            .expect("an opening bid that no entry describes")
    }
}

/// The page that the next call of an auction is on.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Stage {
    /// Nobody has bid.
    Opening,
    /// The next player's side opened with a bid of this group, and has had
    /// this many turns since, counting no more than `Stage::LATER`.
    Ours(usize, usize),
    /// The other side opened with a bid of this group, and the next player's
    /// side has had this many turns since it first did anything but pass.
    Theirs(usize, usize),
}

impl Stage {
    /// The turns from which a side's calls share a page.
    const LATER: usize = 4;

    fn of(history: &[Bid], openings: &Openings) -> Self {
        let calls = from_opening(history);
        let n = calls.len();
        if n == 0 {
            return Stage::Opening;
        }
        let group = openings.group(calls[0]);
        if n.is_multiple_of(2) {
            return Stage::Ours(group, (n / 2).min(Stage::LATER));
        }
        // The next player's side made every second call after the opening.
        let turns = match (1..n).step_by(2).find(|&i| calls[i] != Bid::Pass) {
            Some(first) => (n - first) / 2,
            None => 0,
        };
        Stage::Theirs(group, turns.min(2))
    }
    /// The page's title, and the same as part of an anchor.
    fn title(self) -> (&'static str, &'static str) {
        match self {
            Stage::Opening => ("Opening bids", "bids"),
            Stage::Ours(_, 1) => ("Responses", "responses"),
            Stage::Ours(_, 2) => ("Opener's rebid", "opener-rebid"),
            Stage::Ours(_, 3) => ("Responder's rebid", "responder-rebid"),
            Stage::Theirs(_, 0) => ("Coming into the auction", "coming-in"),
            Stage::Theirs(_, 1) => ("Answering partner", "answering"),
            Stage::Ours(..) | Stage::Theirs(..) => ("Later calls", "later"),
        }
    }
    /// The chapter's title and anchor.
    fn chapter(self, openings: &Openings) -> (Text, String) {
        let bids = |group: usize| or(openings.0[group].iter().copied());
        let slug = |group: usize| bids_string(&openings.0[group][..1]).to_lowercase();
        match self {
            Stage::Opening => (Text::from("Opening the bidding"), "opening".to_string()),
            Stage::Ours(group, _) => (
                Text::from("After opening") + bids(group),
                format!("opened-{}", slug(group)),
            ),
            Stage::Theirs(group, _) => (
                Text::from("When the opponents open") + bids(group),
                format!("over-{}", slug(group)),
            ),
        }
    }
}

/// A call of an auction with its suit left open.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Slot {
    Pass,
    Double,
    Redouble,
    /// A bid at this level, in notrump if the second number is `NOTRUMP`,
    /// and otherwise in the suit of that number among the auction's suits,
    /// counted in the order they were first bid.
    Bid(usize, usize),
}

const NOTRUMP: usize = 4;

/// One auction of a pattern, as a number for each way that the auctions of
/// a pattern differ: the passes before the opening bid, then what each call
/// before the last means (see [`Meanings::of`]), then each of the pattern's
/// suits.
type Filling = Vec<usize>;

/// The calls from the opening bid on with their suits left open, and the
/// suits.
fn pattern(calls: &[Bid]) -> (Vec<Slot>, Vec<Suit>) {
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

/// What the calls of a table's auctions mean: for each call that something
/// describes, the first entry that does.  The entries are the table's own,
/// and after them the invitations of its contract rules (see [`described`]).
struct Meanings<'a> {
    table: &'a Convention,
    ruled: Vec<Convention>,
    /// The entry for the last call of each auction, by `bids_string`.
    first: HashMap<String, usize>,
    /// For each entry, the first entry with the same name that says the
    /// same of itself.
    alike: Vec<usize>,
}

impl<'a> Meanings<'a> {
    fn new(table: &'a Convention, ruled: Vec<Convention>, nodes: &[Node]) -> Self {
        let mut first = HashMap::new();
        for node in nodes {
            for &(i, bid) in &node.described {
                let auction = bids_string(&followed_by(&node.history, bid));
                first.entry(auction).or_insert(i);
            }
        }
        let mut named: HashMap<(&str, &str), usize> = HashMap::new();
        let entries = table.entries().iter().chain(&ruled).enumerate();
        let alike = entries
            .map(|(i, entry)| {
                let said = description(entry).map_or("", |said| said.as_str());
                *named.entry((entry.the_name(), said)).or_insert(i)
            })
            .collect();
        Meanings {
            table,
            ruled,
            first,
            alike,
        }
    }
    /// The entry with this number.
    fn entry(&self, i: usize) -> &Convention {
        let entries = self.table.entries();
        entries
            .get(i)
            .unwrap_or_else(|| &self.ruled[i - entries.len()])
    }
    /// A number for what each call of `history` from the opening bid on
    /// means.  Two calls have the same number when their entries have one
    /// name and say the same of themselves.  It is zero for a call that no
    /// entry describes, and for the calls of the opponents of the player
    /// who is next to call, which are theirs to explain.
    fn of(&self, history: &[Bid]) -> Vec<usize> {
        let n = history.len();
        let passes = n - from_opening(history).len();
        let meaning = |i: usize| match self.first.get(&bids_string(&history[..=i])) {
            Some(&entry) if (n - i).is_multiple_of(2) => self.alike[entry] + 1,
            _ => 0,
        };
        (passes..n).map(meaning).collect()
    }
    /// The name of one of those `meanings`, with what its entry says of
    /// itself when another of them has the same name.
    fn name(&self, meaning: usize, meanings: &BTreeSet<usize>) -> String {
        let Some(entry) = meaning.checked_sub(1).map(|i| self.entry(i)) else {
            return "no agreement".to_string();
        };
        let name = entry.the_name();
        let shared = meanings.iter().any(|&other| {
            other != meaning && other > 0 && self.entry(other - 1).the_name() == name
        });
        let mut written = format_as!(HTML, name).into_string();
        match description(entry) {
            Some(said) if shared && !said.as_str().is_empty() => {
                written += &format!(" ({})", said.as_str());
            }
            _ => {}
        }
        written
    }
}

/// The seats of players who call after this many passes, as in "first or
/// second seat".
fn seats(passes: impl IntoIterator<Item = usize>) -> Text {
    const SEATS: [&str; 4] = ["first", "second", "third", "fourth"];
    or(passes.into_iter().map(|p| SEATS[p])) + "seat"
}

/// One of the calls that a call of a [`Line`] may be.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Choice {
    slot: Slot,
    /// The call as it is written, as in "2Z" or "(Dbl)".
    call: String,
    /// What the call means, when that needs saying.
    note: Option<String>,
    /// Whether the call is written with a few suits, like 2♦/♥.
    several: bool,
}

/// Some auctions as bridge players write them, as in "1X – (Dbl) – 1Y",
/// where the last call is the one being described.  The calls of that
/// player's opponents are in parentheses, and are left out when they are
/// all passes.
#[derive(Clone)]
struct Line {
    /// The calls that are written, each with every call it may be, as in
    /// "1X – 1Y – 1NT/2X – 2NT".
    calls: Vec<Vec<Choice>>,
    /// The seats that the opening bid may be in, when that needs saying.
    seats: Option<String>,
    /// More that the line is true of, as in "or over (Dbl)".
    also: String,
    /// The length and the first of the auctions, to put lines in order.
    first: (usize, Vec<Slot>, Vec<usize>),
}

impl Line {
    /// How many notes follow the auction.
    fn notes(&self) -> usize {
        let noted = self.calls.iter().flatten();
        noted.filter(|choice| choice.note.is_some()).count() + self.seats.iter().count()
    }
    /// This line and `other` as one, when all that differs is what one of
    /// their calls may be.
    fn with(&self, other: &Line) -> Option<Line> {
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
    fn html(&self) -> FormattedString<HTML> {
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
fn merged(mut lines: Vec<Line>) -> Vec<Line> {
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
fn write(
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
    for (number, d) in (last + 1..values.len()).enumerate() {
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
    for (number, d) in (last + 1..values.len()).enumerate() {
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
        if i < last && !open[i + 1] {
            let mut meant = values[i + 1].iter();
            let (Some(&meaning), None) = (meant.next(), meant.next()) else {
                return None;
            };
            let others: BTreeSet<usize> = all.iter().map(|filling| filling[i + 1]).collect();
            note = Some(meanings.name(meaning, &others));
        }
        calls.push(vec![Choice {
            slot: slots[i],
            call: if theirs(i) { format!("({call})") } else { call },
            note,
            several,
        }]);
    }
    let seats = (!open[0]).then(|| {
        let when = if last == 0 { "in" } else { "opened in" };
        let seats = Text::from(when) + seats(values[0].iter().copied());
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
            (last + 1..values.len()).map(suits).collect(),
        ),
    })
}

/// The fewest ways found of writing the auctions `mine` of one pattern,
/// which are some of the auctions `all` of that pattern that a player might
/// look up.  Nothing is written that the other auctions make plain: a suit
/// is a letter when the auctions with every other suit there are meant too.
fn written(slots: &[Slot], mine: &[Filling], all: &[Filling], meanings: &Meanings) -> Vec<Line> {
    let last = slots.len() - 1;
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
        let suits_first = (last + 1..dims).chain(1..=last).chain([0]);
        let meanings_first = (1..dims).chain([0]);
        for order in [meanings_first.collect::<Vec<_>>(), suits_first.collect()] {
            let mut open = vec![false; dims];
            // A single suit is as short as a letter, and says more.
            for d in order
                .into_iter()
                .filter(|&d| d <= last || values[d].len() > 1)
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
    let apart = [several(1..last + 1), several(last + 1..dims), several(0..1)];
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

/// Other words for the suits of an auction, for the player who makes its
/// last call, as in "partner's suit".  A suit belongs to the side that bid
/// it first.
fn roles(calls: &[Bid]) -> HashMap<Suit, &'static str> {
    const YOURS: [&str; 3] = ["your suit", "your first suit", "your second suit"];
    const PARTNERS: [&str; 3] = [
        "partner's suit",
        "partner's first suit",
        "partner's second suit",
    ];
    const THEIRS: [&str; 3] = ["their suit", "their first suit", "their second suit"];
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

/// What one way of fitting a bid asks of a hand.
fn fit_features(fit: &Fit) -> Vec<Feature> {
    let mut features = describe(&fit.min, &fit.max);
    let balanced = matches!(features.first(), Some(Feature::Shape(ShapeRule::Balanced)));
    features.extend(
        fit.shape
            .iter()
            .filter(|&&rule| !(balanced && rule == ShapeRule::Balanced))
            .map(|&rule| Feature::Shape(rule)),
    );
    features
}

/// What an entry of `table` asks of a hand that makes the last of `calls`,
/// for each way of fitting it, or nothing when any hand fits.  A limit bid
/// has at most the points that the rest of the table leaves it.
fn features(table: &Convention, entry: &Convention, calls: &[Bid]) -> Vec<Vec<Feature>> {
    let limited = |fit: &Fit| match fit.limit {
        true => fit_features(&table.limit(calls, fit)),
        false => fit_features(fit),
    };
    let fits = match entry {
        Convention::Simple { min, max, .. } => vec![describe(min, max)],
        Convention::Rule { fits, .. } => fits.iter().map(limited).collect(),
        _ => Vec::new(),
    };
    match fits.iter().any(|features| features.is_empty()) {
        true => Vec::new(),
        false => fits,
    }
}

/// What an entry says about itself.
fn description(entry: &Convention) -> Option<&FormattedString<HTML>> {
    match entry {
        Convention::Simple {
            the_description, ..
        }
        | Convention::Rule {
            the_description, ..
        } => Some(the_description),
        _ => None,
    }
}

/// The hands that `features` ask for, one text for each way of fitting,
/// where `called` has other words for some suits.
fn hands(features: &[Vec<Feature>], called: &dyn Fn(Suit) -> Option<&'static str>) -> Vec<Text> {
    features
        .iter()
        .map(|fit| and(fit.iter().map(|feature| feature.text_calling(called))))
        .collect()
}

/// A call that an entry describes, in its auction.
struct Call {
    entry: usize,
    /// The calls from the opening bid on, ending with this one.
    calls: Vec<Bid>,
    /// The same as a pattern and the way this auction fills it.
    slots: Vec<Slot>,
    filling: Filling,
}

/// `slots` with a pass for each double and redouble by the opponents of the
/// player who makes the last call.
fn undoubled(slots: &[Slot]) -> Vec<Slot> {
    let last = slots.len() - 1;
    let quiet = |(i, &slot): (usize, &Slot)| match slot {
        Slot::Double | Slot::Redouble if (last - i) % 2 == 1 => Slot::Pass,
        _ => slot,
    };
    slots.iter().enumerate().map(quiet).collect()
}

/// Whether no hand has as many cards in each suit as `min` asks for, which
/// is how the table says never to make a bid.
fn fits_no_hand(min: &HandValuation) -> bool {
    let cards = Suit::ALL.iter().map(|&suit| usize::from(min.length[suit]));
    cards.sum::<usize>() > 13
}

/// Whether no hand fits an entry.
fn is_never_bid(entry: &Convention) -> bool {
    match entry {
        Convention::Simple { min, .. } => fits_no_hand(min),
        Convention::Rule { fits, .. } => fits.iter().all(|fit| fits_no_hand(&fit.min)),
        _ => false,
    }
}

/// The calls that mean the same, with the hand they show.
struct Row {
    /// The auctions, each ending in the call.
    auctions: Vec<FormattedString<HTML>>,
    name: &'static str,
    /// What the entries say about themselves, when they all say the same
    /// and it is more than the name.
    description: Option<FormattedString<HTML>>,
    forcing: Forcing,
    /// The hands that fit, one text for each way of fitting, or none when
    /// any hand fits.
    hands: Vec<Text>,
}

impl Row {
    /// The line for `calls`, which mean the same, where `all` are the
    /// auctions of each pattern that a player might look up on this page.
    fn new(meanings: &Meanings, calls: &[&Call], all: &HashMap<Vec<Slot>, Vec<Filling>>) -> Self {
        let entry = meanings.entry(calls[0].entry);
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
            let seats: BTreeSet<usize> = all[slots].iter().map(|filling| filling[0]).collect();
            let seated = |fillings: &[Filling]| -> Vec<Filling> {
                let seated = fillings
                    .iter()
                    .filter(|filling| seats.contains(&filling[0]));
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
        let said = description(entry).filter(|said| {
            !said.as_str().is_empty()
                && said.as_str() != entry.the_name()
                && calls
                    .iter()
                    .all(|call| description(meanings.entry(call.entry)) == Some(*said))
        });
        Row {
            auctions,
            name: entry.the_name(),
            description: said.cloned(),
            forcing: entry.is_forcing(),
            hands: match is_never_bid(entry) {
                true => vec![Text::from("no hand: this is never bid")],
                false => hands(&features(meanings.table, entry, &calls[0].calls), &called),
            },
        }
    }
}

impl Row {
    /// Whether this line has the same meaning and hands as `other`.
    fn says_the_same_as(&self, other: &Row) -> bool {
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
struct Page {
    id: String,
    title: &'static str,
    rows: Vec<Row>,
}

impl Page {
    /// The page for `stage`, whose auctions are `nodes`.
    fn new(meanings: &Meanings, stage: Stage, id: String, nodes: &[&Node]) -> Self {
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
            let features = features(meanings.table, entry, &call.calls);
            let hands = hands(&features, &|suit| roles.get(&suit).copied());
            let hands: Vec<String> = hands
                .iter()
                .map(|hand| format_as!(HTML, hand).into_string())
                .collect();
            let meaning = format!(
                "{} {:?} {}",
                entry.the_name(),
                entry.is_forcing(),
                hands.join(" or ")
            );
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

/// A chapter of the booklet: the bidding after some opening bids.
struct Part {
    id: String,
    title: Text,
    pages: Vec<Page>,
}

/// What a partnership needs between them for a bid that no other entry
/// describes.
struct Natural {
    bids: Vec<Bid>,
    name: &'static str,
    needs: Text,
}

/// A kind of contract, and the points the contract rules need between the
/// two hands to bid it.
struct Needed {
    kind: &'static str,
    points: u8,
}

/// What the contract rules need for each kind of contract, or nothing when
/// `table` does not bid by them.
fn contracts(table: &Convention) -> Vec<Needed> {
    let entries = table.entries().iter();
    let kinds = [
        ("Game in notrump or a major", Bid::NT(3)),
        ("Game in a minor", Bid::Suit(5, Suit::Clubs)),
        ("Small slam", Bid::NT(6)),
        ("Grand slam", Bid::NT(7)),
    ];
    let needed = |(kind, bid)| Needed {
        kind,
        points: contract_points(bid).expect("a game or a slam"),
    };
    match entries
        .into_iter()
        .any(|entry| matches!(entry, Convention::Contract { .. }))
    {
        true => kinds.into_iter().map(needed).collect(),
        false => Vec::new(),
    }
}

/// A booklet on a bidding system.
pub struct Booklet {
    system: BiddingSystem,
    parts: Vec<Part>,
    /// What the contract rules need, if the system bids by them.
    contracts: Vec<Needed>,
    natural: Vec<Natural>,
}

#[with_template("[%" "%]" "booklet.html")]
impl DisplayAs<HTML> for Booklet {}

impl Booklet {
    /// The booklet on `system`.
    pub fn new(system: BiddingSystem) -> Self {
        let table = system.convention();
        let (nodes, ruled) = walk(&table);
        let meanings = Meanings::new(&table, ruled, &nodes);
        let openings = Openings::new(&meanings, &nodes);
        let mut stages: BTreeMap<Stage, Vec<&Node>> = BTreeMap::new();
        for node in &nodes {
            let stage = Stage::of(&node.history, &openings);
            stages.entry(stage).or_default().push(node);
        }
        let mut parts: Vec<Part> = Vec::new();
        for (stage, nodes) in stages {
            if nodes.iter().all(|node| node.described.is_empty()) {
                continue;
            }
            let (title, id) = stage.chapter(&openings);
            if parts.last().is_none_or(|part| part.id != id) {
                parts.push(Part {
                    id,
                    title,
                    pages: Vec::new(),
                });
            }
            let part = parts.last_mut().unwrap();
            let id = format!("{}-{}", part.id, stage.title().1);
            part.pages.push(Page::new(&meanings, stage, id, &nodes));
        }
        Booklet {
            system,
            parts,
            contracts: contracts(&table),
            natural: naturals(&table),
        }
    }
}

/// The table's defaults, for bids that no other entry describes: what the
/// partnership needs between them for each.  Bids that need the same in
/// their own suits share a line.
fn naturals(table: &Convention) -> Vec<Natural> {
    const CARD: Word = Word::noun("card", "cards");
    let mut naturals: Vec<Natural> = Vec::new();
    for entry in table.entries() {
        let (name, mut min) = match entry {
            Convention::Natural { the_name, min, .. }
            | Convention::Forced { the_name, min, .. } => (*the_name, *min),
            _ => continue,
        };
        let patterns = entry.regex().into_iter().flat_map(|regex| regex.patterns());
        for bid in patterns.filter_map(|pattern| final_bid(pattern)) {
            let mut needs = Vec::new();
            if let Bid::Suit(_, suit) = bid {
                if min.length[suit] > 0 {
                    needs.push(at_least(min.length[suit], CARD) + "in the suit");
                    min.length[suit] = 0;
                }
            }
            let rest = describe(&min, &HandValuation::MAX);
            needs.extend(rest.into_iter().map(Feature::text));
            if needs.is_empty() {
                continue;
            }
            let needs = and(needs);
            let html = |text: &Text| format_as!(HTML, text).into_string();
            match naturals
                .iter_mut()
                .find(|n| n.name == name && html(&n.needs) == html(&needs))
            {
                Some(natural) => natural.bids.push(bid),
                None => naturals.push(Natural {
                    bids: vec![bid],
                    name,
                    needs,
                }),
            }
        }
    }
    for natural in naturals.iter_mut() {
        natural.bids.sort_by_key(|&bid| rank(bid));
        natural.bids.dedup();
    }
    naturals.sort_by_key(|n| rank(n.bids[0]));
    naturals
}

#[cfg(test)]
mod tests {
    use super::*;

    fn html(system: BiddingSystem) -> String {
        format_as!(HTML, Booklet::new(system)).into_string()
    }

    /// The text between each `before` and the next double quote.
    fn quoted<'a>(html: &'a str, before: &str) -> Vec<&'a str> {
        html.split(before)
            .skip(1)
            .map(|rest| rest.split('"').next().unwrap())
            .collect()
    }

    /// The page of `html` with this id.
    fn page<'a>(html: &'a str, id: &str) -> &'a str {
        let start = html
            .find(&format!("<section class=\"page\" id=\"{id}\">"))
            .unwrap_or_else(|| panic!("no page {id}"));
        let rest = &html[start..];
        &rest[..rest.find("</section>").unwrap()]
    }

    /// The words of the row of `page` whose meaning column starts with `name`.
    fn row(page: &str, name: &str) -> String {
        let row = page
            .split("<tr>")
            .find(|row| row.contains(&format!("<td class=\"meaning\">{name}")))
            .unwrap_or_else(|| panic!("no row {name} in {page}"));
        words(row)
    }

    /// The text of `html` without its tags.
    fn words(html: &str) -> String {
        let pieces = html.split('<').enumerate();
        let pieces = pieces.map(|(i, piece)| match piece.split_once('>') {
            Some((_tag, text)) if i > 0 => text,
            _ => piece,
        });
        pieces.collect()
    }

    #[test]
    fn every_link_goes_somewhere() {
        for system in BiddingSystem::ALL {
            let html = html(system);
            let ids = quoted(&html, " id=\"");
            let unique: HashSet<&str> = ids.iter().copied().collect();
            assert_eq!(
                ids.len(),
                unique.len(),
                "{}: an id is used twice",
                system.name()
            );
            let links = quoted(&html, " href=\"#");
            assert!(
                links.len() > 15,
                "{}: only {} links",
                system.name(),
                links.len()
            );
            for link in links {
                assert!(
                    unique.contains(link),
                    "{}: nothing has the id {link}",
                    system.name()
                );
            }
        }
    }

    #[test]
    fn the_same_booklet_every_time() {
        assert!(html(BiddingSystem::Sheets) == html(BiddingSystem::Sheets));
    }

    /// The booklet is one small file, not a page for every auction.
    #[test]
    fn a_booklet_is_small() {
        for system in BiddingSystem::ALL {
            let html = html(system);
            let pages = html.matches("<section class=\"page\"").count();
            assert!(pages < 30, "{}: {pages} pages", system.name());
            assert!(
                html.len() < 150_000,
                "{}: {} bytes",
                system.name(),
                html.len()
            );
        }
    }

    #[test]
    fn the_responses_to_a_strong_two_clubs() {
        let html = html(BiddingSystem::Sayc);
        assert!(
            html.contains("<a class=\"page\" href=\"#opened-2c-responses\">"),
            "the responses to 2♣ are not in the contents"
        );
        let responses = page(&html, "opened-2c-responses");
        let weak = row(responses, "Weak response");
        assert!(weak.starts_with("2♣ – 2♦\n"), "{weak}");
        let strong = row(responses, "Strong response");
        assert!(strong.contains("game forcing"), "{strong}");
        page(&html, "opened-2c-opener-rebid");
    }

    #[test]
    fn opening_one_notrump() {
        let html = html(BiddingSystem::Sayc);
        let opening = row(page(&html, "opening-bids"), "Opening 1NT");
        assert!(opening.contains("15–17 high card points"), "{opening}");
        let responses = page(&html, "opened-1n-responses");
        let stayman = row(responses, "Stayman");
        assert!(stayman.starts_with("1NT – 2♣\n"), "{stayman}");
        row(responses, "Jacoby transfer");
    }

    /// Auctions that differ only in their suits are written once, with letters.
    #[test]
    fn suits_are_written_as_letters() {
        let html = html(BiddingSystem::Sayc);
        let responses = words(page(&html, "opened-1c-responses"));
        assert!(responses.contains("\n1X – Pass\n"), "{responses}");
        assert!(responses.contains("\n1M – 2M\n"), "{responses}");
    }

    /// Only the systems that bid by the contract rules explain them.
    #[test]
    fn the_contract_rules_are_explained() {
        let rules = "<h3 id=\"contracts\">";
        assert!(!html(BiddingSystem::Sheets).contains(rules));
        let html = html(BiddingSystem::Sayc);
        let rules = words(&html[html.find(rules).expect("no contract rules")..]);
        assert!(
            rules.contains("Game in notrump or a major\n25\n"),
            "{rules}"
        );
    }

    #[test]
    fn natural_bids_say_what_the_side_needs() {
        let html = html(BiddingSystem::Sheets);
        let natural = &html[html.find("<h2 id=\"natural\">").unwrap()..];
        assert!(natural.contains("26+ high card points"), "{natural}");
    }

    /// Writes each system's booklet to `{BOOKLET_DIR}/{slug}.html`, to look at.
    #[test]
    #[ignore]
    fn write_booklets() {
        let dir = std::env::var("BOOKLET_DIR").expect("set BOOKLET_DIR");
        for system in BiddingSystem::ALL {
            let start = std::time::Instant::now();
            let booklet = Booklet::new(system);
            let pages = booklet.parts.iter().flat_map(|part| &part.pages);
            let pages: Vec<usize> = pages.map(|page| page.rows.len()).collect();
            let html = format_as!(HTML, booklet).into_string();
            println!(
                "{}: {} pages, {} lines, {} bytes in {:?}",
                system.name(),
                pages.len(),
                pages.iter().sum::<usize>(),
                html.len(),
                start.elapsed()
            );
            std::fs::write(format!("{dir}/{}.html", system.slug()), html).unwrap();
        }
    }
}
