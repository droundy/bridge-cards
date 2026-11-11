//! Following a table from the first call: the auctions it describes, what
//! each call of them means, and the page each is on.

use crate::ai::advice::{describe, Feature};
use crate::ai::rules::Rule;
use crate::ai::{bids_string, legal_bids, Convention, Fit, Forcing, ShapeRule};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use display_as::{format_as, HTML};
use std::collections::{BTreeSet, HashMap, HashSet};
use wording::{or, Text};

/// Where a call stands among all calls, lowest first.
pub(super) fn rank(bid: Bid) -> usize {
    match bid {
        Bid::Pass => 0,
        Bid::Double => 1,
        Bid::Redouble => 2,
        Bid::Suit(level, suit) => 5 * level + suit as usize,
        Bid::NT(level) => 5 * level + 4,
    }
}

/// Is the auction over?
pub(super) fn is_over(history: &[Bid]) -> bool {
    let n = history.len();
    n >= 4 && history[n - 3..].iter().all(|&b| b == Bid::Pass)
}

/// `history` followed by `call`.
pub(super) fn followed_by(history: &[Bid], call: Bid) -> Vec<Bid> {
    [history, &[call]].concat()
}

/// `history` from its first call that is not a pass.
pub(super) fn from_opening(history: &[Bid]) -> &[Bid] {
    let passes = history.iter().take_while(|&&b| b == Bid::Pass).count();
    &history[passes..]
}

/// What a call means: its name, what it says of itself, and the hands that
/// make it.
pub(super) struct Meaning {
    pub(super) name: &'static str,
    /// What it says of itself beyond its name, or nothing.
    pub(super) said: String,
    pub(super) forcing: Forcing,
    /// The ways a hand can fit.
    pub(super) fits: Vec<Fit>,
}

impl Meaning {
    /// What an entry of a table means.
    pub(super) fn of_entry(entry: &Convention) -> Self {
        let (said, fits) = match entry {
            Convention::Simple {
                the_description,
                min,
                max,
                ..
            } => (the_description.as_str(), vec![Fit::new(*min, *max)]),
            Convention::Rule {
                the_description,
                fits,
                ..
            } => (the_description.as_str(), fits.clone()),
            _ => ("", Vec::new()),
        };
        Meaning {
            name: entry.the_name(),
            said: said.to_string(),
            forcing: entry.is_forcing(),
            fits,
        }
    }
    /// An invitation that the contract rules make of a bid, which says of
    /// itself what it invites, or nothing for any other `meaning`.
    pub(super) fn of_invitation(meaning: &Convention) -> Option<Self> {
        match meaning {
            Convention::Contracted {
                the_name,
                min,
                max,
                reason,
                ..
            } if reason.rule == Rule::Invite => Some(Meaning {
                name: the_name,
                said: format_as!(HTML, reason.aim()).into_string(),
                forcing: Forcing::Passable,
                fits: vec![Fit::new(*min, *max)],
            }),
            _ => None,
        }
    }
    /// Whether no hand fits, which is how a table says never to make a bid.
    pub(super) fn is_never_bid(&self) -> bool {
        !self.fits.is_empty() && self.fits.iter().all(|fit| fits_no_hand(&fit.min))
    }
    /// What this asks of a hand that makes the last of `bids` by `table`,
    /// for each way of fitting, or nothing when any hand fits.  A limit bid
    /// has at most the points that the rest of the table leaves it.
    pub(super) fn features(&self, table: &Convention, bids: &[Bid]) -> Vec<Vec<Feature>> {
        let features = |fit: &Fit| match fit.limit {
            true => fit_features(&table.limit(bids, fit)),
            false => fit_features(fit),
        };
        let fits: Vec<Vec<Feature>> = self.fits.iter().map(features).collect();
        match fits.iter().any(|features| features.is_empty()) {
            true => Vec::new(),
            false => fits,
        }
    }
}

/// What says outright what a call after `history` shows, each with its
/// call: the entries of `table` by their place in it, in its order, and then
/// each invitation that its contract rules make of a response to an opening
/// bid that no entry describes, which goes on the end of `invitations` and
/// is numbered from the end of the table.  That leaves out the `Natural` and
/// `Forced` entries, which are defaults for any auction, the contracts that
/// the rules place, and their invitations later in the auction, which would
/// fill the later pages with what the rules say once.
pub(super) fn described(
    table: &Convention,
    history: &[Bid],
    invitations: &mut Vec<Meaning>,
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
        let meaning = table.refine(&bids);
        if let Some(invitation) = meaning.as_ref().and_then(Meaning::of_invitation) {
            found.push((entries.len() + invitations.len(), bid));
            invitations.push(invitation);
        }
    }
    found.sort_by_key(|&(i, _)| i);
    found
}

/// An auction that the start of the auction leads to by calls the table
/// describes, where a player the table has nothing for passes once.
pub(super) struct Node {
    pub(super) history: Vec<Bid>,
    /// What describes the next call, each with its call: see [`described`].
    pub(super) described: Vec<(usize, Bid)>,
}

/// Every such auction, and the invitations in them that the contract rules
/// make of bids no entry describes.
pub(super) fn walk(table: &Convention) -> (Vec<Node>, Vec<Meaning>) {
    let mut nodes = Vec::new();
    let mut invitations = Vec::new();
    let mut seen = HashSet::new();
    // Each auction to look at, and whether its last call is a pass that no
    // entry describes.
    let mut todo = vec![(Vec::new(), false)];
    while let Some((history, assumed)) = todo.pop() {
        if is_over(&history) || !seen.insert(bids_string(&history)) {
            continue;
        }
        let described = described(table, &history, &mut invitations);
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
    (nodes, invitations)
}

/// The opening bids, in groups that share their pages: the bids of one level
/// that the table gives the same name, such as the weak twos.
pub(super) struct Openings(pub(super) Vec<Vec<Bid>>);

impl Openings {
    pub(super) fn new(meanings: &Meanings, nodes: &[Node]) -> Self {
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
            let key = (level, meanings.entry(i).name);
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
    pub(super) fn group(&self, bid: Bid) -> usize {
        self.0
            .iter()
            .position(|bids| bids.contains(&bid))
            .expect("an opening bid that no entry describes")
    }
}

/// The page that the next call of an auction is on.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Stage {
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
    pub(super) const LATER: usize = 4;

    pub(super) fn of(history: &[Bid], openings: &Openings) -> Self {
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
    pub(super) fn title(self) -> (&'static str, &'static str) {
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
    pub(super) fn chapter(self, openings: &Openings) -> (Text, String) {
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

/// What the calls of a table's auctions mean: for each call that something
/// describes, the first entry that does.  The entries are the table's own,
/// and after them the invitations of its contract rules (see [`described`]).
pub(super) struct Meanings<'a> {
    pub(super) table: &'a Convention,
    /// What each entry means, and then each invitation.
    pub(super) entries: Vec<Meaning>,
    /// Whether there are any invitations.
    pub(super) invites: bool,
    /// The entry for the last call of each auction, by `bids_string`.
    pub(super) first: HashMap<String, usize>,
    /// For each entry, the first entry with the same name that says the
    /// same of itself.
    pub(super) alike: Vec<usize>,
}

impl<'a> Meanings<'a> {
    pub(super) fn new(table: &'a Convention, invitations: Vec<Meaning>, nodes: &[Node]) -> Self {
        let mut first = HashMap::new();
        for node in nodes {
            for &(i, bid) in &node.described {
                let auction = bids_string(&followed_by(&node.history, bid));
                first.entry(auction).or_insert(i);
            }
        }
        let invites = !invitations.is_empty();
        let mut entries: Vec<Meaning> = table.entries().iter().map(Meaning::of_entry).collect();
        entries.extend(invitations);
        let mut named: HashMap<(&str, &str), usize> = HashMap::new();
        let alike = entries
            .iter()
            .enumerate()
            .map(|(i, entry)| *named.entry((entry.name, &entry.said)).or_insert(i))
            .collect();
        Meanings {
            table,
            entries,
            invites,
            first,
            alike,
        }
    }
    /// What the entry with this number means.
    pub(super) fn entry(&self, i: usize) -> &Meaning {
        &self.entries[i]
    }
    /// A number for what each call of `history` from the opening bid on
    /// means.  Two calls have the same number when their entries have one
    /// name and say the same of themselves.  It is zero for a call that no
    /// entry describes, and for the calls of the opponents of the player
    /// who is next to call, which are theirs to explain.
    pub(super) fn of(&self, history: &[Bid]) -> Vec<usize> {
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
    pub(super) fn name(&self, meaning: usize, meanings: &BTreeSet<usize>) -> String {
        let Some(entry) = meaning.checked_sub(1).map(|i| self.entry(i)) else {
            return "no agreement".to_string();
        };
        let shared = meanings.iter().any(|&other| {
            other != meaning && other > 0 && self.entry(other - 1).name == entry.name
        });
        let mut written = format_as!(HTML, entry.name).into_string();
        if shared && !entry.said.is_empty() {
            written += &format!(" ({})", entry.said);
        }
        written
    }
}

/// What one way of fitting a bid asks of a hand.
pub(super) fn fit_features(fit: &Fit) -> Vec<Feature> {
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

/// Whether no hand has as many cards in each suit as `min` asks for, which
/// is how the table says never to make a bid.
pub(super) fn fits_no_hand(min: &HandValuation) -> bool {
    let cards = Suit::ALL.iter().map(|&suit| usize::from(min.length[suit]));
    cards.sum::<usize>() > 13
}
