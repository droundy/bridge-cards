//! The table of blocks that opens a page: the points run across it, each row
//! is a kind of hand, and each block is the call the robot makes with such a
//! hand and that many points.  Every cell has exactly one call, so the blocks
//! fill the table, and a call that reaches different points for different
//! hands is a block with steps.
//!
//! Nothing here is written by hand for a bidding system.  The kinds of hand
//! come from the page's own entries: two hands are alike when they pass the
//! same tests on length, shape and every value but the points along the
//! table.  Kinds that the robot treats alike at every count of points share a
//! row, and the rows are put in an order that keeps a call's cells together.
//! Calls are lumped into families, such as the major suit bids of one level,
//! so that "1♥ / 1♠" is one block.
//!
//! The row model says which call each kind of hand makes at each count of
//! points, and nothing about height, which means nothing.  `layout` makes the
//! blocks from it, one for each run of counts of points that a family of calls
//! covers, whatever the rows, and packs them as tightly as they go, with no
//! luck involved: see its docs.
//!
//! Planned: a flow chart for the same page, a way to decide your call by
//! asking questions.  Each question is the one that splits what is left of the
//! hands closest to half and half, so that the most common outcomes are not
//! always asked first, and it is built from the same samples, so that the chart
//! and the table cannot disagree.

use super::html::escape;
use super::{Booklet, Page, ShapeRule, POINTS};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use std::cmp::Reverse;
use std::collections::HashMap;

mod layout;
use layout::Layout;

/// A number a hand has, other than its points.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Dim {
    Nltc2,
    Length(Suit),
    HcpIn(Suit),
    HcpOutside(Suit),
}

impl Dim {
    fn all() -> Vec<Dim> {
        let mut dims = vec![Dim::Nltc2];
        for suit in Suit::ALL {
            dims.extend([Dim::Length(suit), Dim::HcpIn(suit), Dim::HcpOutside(suit)]);
        }
        dims
    }
    fn of(self, hand: &HandValuation) -> u8 {
        match self {
            Dim::Nltc2 => hand.nltc2,
            Dim::Length(suit) => hand.length[suit],
            Dim::HcpIn(suit) => hand.hcp_in_suit[suit],
            Dim::HcpOutside(suit) => hand.hcp_outside_suit[suit],
        }
    }
}

/// A test that some entry of a page makes of a hand.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Feature {
    AtLeast(Dim, u8),
    Shape(ShapeRule),
}

impl Feature {
    fn holds(self, hand: &HandValuation) -> bool {
        match self {
            Feature::AtLeast(dim, n) => dim.of(hand) >= n,
            Feature::Shape(rule) => rule.holds(hand),
        }
    }
}

/// How many tests tell hands apart, at most.
const FEATURES: usize = 128;

/// The tests that the entries of `decisions` make of a hand, other than those
/// on its points, which the table runs along.
pub(super) fn features(decisions: &[super::Decision]) -> Vec<Feature> {
    let mut found: Vec<Feature> = Vec::new();
    let mut add = |feature: Feature| {
        if !found.contains(&feature) {
            found.push(feature);
        }
    };
    for fit in decisions.iter().flat_map(|d| d.fits.iter()) {
        for dim in Dim::all() {
            if dim.of(&fit.min) > dim.of(&HandValuation::MIN) {
                add(Feature::AtLeast(dim, dim.of(&fit.min)));
            }
            if dim.of(&fit.max) < dim.of(&HandValuation::MAX) {
                add(Feature::AtLeast(dim, dim.of(&fit.max) + 1));
            }
        }
        for &rule in fit.shape.iter() {
            add(Feature::Shape(rule));
        }
    }
    found.truncate(FEATURES);
    found
}

/// What tells hands apart in the table: which of the tests of the page a hand
/// passes, one bit each.  Points are not among them, since the table runs
/// along them.
pub(super) type Kind = u128;

/// `hand` with `points` of every kind, as bidders talk of points: the table
/// does not say which kind a call counts, and the calls that count different
/// kinds meet at the same number.
fn with_points(hand: &HandValuation, points: usize) -> HandValuation {
    let points = points.min(POINTS - 1) as u8;
    HandValuation {
        hcp: points,
        lhcp: points,
        shcp: points,
        ..*hand
    }
}

/// The kind of `hand`.
pub(super) fn kind(features: &[Feature], hand: &HandValuation) -> Kind {
    features
        .iter()
        .enumerate()
        .filter(|(_, f)| f.holds(hand))
        .fold(0, |bits, (i, _)| bits | 1 << i)
}

/// Calls that a block lumps together.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Family(usize);

/// The families of a page.  Calls that the same kind of entry describes, at
/// the same level, are one family: the weak twos, the Jacoby transfers, the
/// opening bids of one suit.
struct Families {
    of: HashMap<Bid, Family>,
    /// What the entries of each family are called.
    names: Vec<String>,
    /// The class of the colour of its blocks.
    classes: Vec<&'static str>,
}

impl Families {
    fn new(page: &Page) -> Families {
        let mut found = Families {
            of: HashMap::new(),
            names: vec!["Pass".to_string()],
            classes: vec!["k-pass"],
        };
        let mut by_key: HashMap<(&str, Option<usize>), usize> = HashMap::new();
        found.of.insert(Bid::Pass, Family(0));
        for d in page.decisions.iter().filter(|d| d.call != Bid::Pass) {
            let id = *by_key
                .entry((d.name.as_str(), d.call.level()))
                .or_insert_with(|| {
                    found.names.push(d.name.clone());
                    found.classes.push("");
                    found.names.len() - 1
                });
            found.of.entry(d.call).or_insert(Family(id));
        }
        // Colour by what its calls are: notrump, major suits, minor suits,
        // a mix of suits, or a double.
        let mut calls: HashMap<usize, Vec<Bid>> = HashMap::new();
        for (&bid, family) in found.of.iter() {
            calls.entry(family.0).or_default().push(bid);
        }
        for (id, calls) in calls {
            if id == 0 {
                continue;
            }
            let major = |b: &Bid| matches!(b, Bid::Suit(_, Suit::Hearts | Suit::Spades));
            let minor = |b: &Bid| matches!(b, Bid::Suit(_, Suit::Clubs | Suit::Diamonds));
            found.classes[id] = if calls.iter().all(|b| matches!(b, Bid::NT(_))) {
                "k-nt"
            } else if calls.iter().all(major) {
                "k-major"
            } else if calls.iter().all(minor) {
                "k-minor"
            } else if calls.iter().any(|b| matches!(b, Bid::Suit(..) | Bid::NT(_))) {
                "k-suit"
            } else {
                "k-x"
            };
        }
        found
    }
    /// The family of `bid`.  Any call the page does not describe, which can
    /// only be the pass that nothing else beats, is a pass.
    fn of(&self, bid: Bid) -> Family {
        self.of.get(&bid).copied().unwrap_or(Family(0))
    }
}

/// The calls of a family in the order a label lists them.
fn label_order(bid: Bid) -> (usize, u8) {
    match bid {
        Bid::Suit(level, suit) => (level, suit as u8),
        Bid::NT(level) => (level, 4),
        _ => (0, 0),
    }
}

/// The calls of a block as a label, as SVG text and as plain text: "1♥ / 1♠".
fn names(calls: &[&super::Line]) -> (String, String) {
    let join = |f: &dyn Fn(Bid) -> String| {
        calls
            .iter()
            .map(|l| f(l.call))
            .collect::<Vec<_>>()
            .join(" / ")
    };
    (join(&call_svg), join(&super::call))
}

/// The same label shortened, when it must fit in a narrow block, by naming the
/// level once: "1♥/♠".
fn short_names(calls: &[&super::Line]) -> (String, String) {
    if calls.len() < 2 {
        return names(calls);
    }
    let level = calls[0].call.level();
    if calls.iter().any(|l| l.call.level() != level) {
        return names(calls);
    }
    let strains = |f: &dyn Fn(Bid) -> String| {
        calls
            .iter()
            .map(|l| f(l.call))
            .collect::<Vec<_>>()
            .join("/")
    };
    let svg = |bid: Bid| match bid {
        Bid::Suit(_, suit) => format!("<tspan class=\"{}\">{}</tspan>", suit.name(), suit.unicode()),
        _ => "NT".to_string(),
    };
    let plain = |bid: Bid| match bid {
        Bid::Suit(_, suit) => suit.unicode().to_string(),
        _ => "NT".to_string(),
    };
    (
        format!("{}{}", level.unwrap_or(0), strains(&svg)),
        format!("{}{}", level.unwrap_or(0), strains(&plain)),
    )
}

/// A call as SVG text, with its suit in colour.
fn call_svg(bid: Bid) -> String {
    match bid {
        Bid::Suit(level, suit) => format!(
            "{}<tspan class=\"{}\">{}</tspan>",
            level,
            suit.name(),
            suit.unicode()
        ),
        other => escape(&super::call(other)),
    }
}

/// How much hands weigh at one count of points, by the family of call the
/// robot makes with them.
type Cell = HashMap<Family, u64>;

/// Kinds of hand that the table does not yet tell apart: how much they weigh
/// for each family of call, at each count of points.
struct Group {
    mass: u64,
    cells: Vec<Cell>,
}

/// The most rows a table has.
const MAX_ROWS: usize = 10;

impl Group {
    fn merge(&mut self, other: Group) {
        self.mass += other.mass;
        for (mine, theirs) in self.cells.iter_mut().zip(other.cells) {
            for (family, n) in theirs {
                *mine.entry(family).or_default() += n;
            }
        }
    }
    /// The family with the most weight in `cell`.
    fn winner(cell: &Cell) -> Family {
        cell.iter()
            .max_by_key(|&(&family, &n)| (n, Reverse(family)))
            .map_or(Family(0), |(&family, _)| family)
    }
    /// How much weight would disagree with the winning calls if `other` joined
    /// this group.
    fn loss(&self, other: &Group) -> u64 {
        self.cells
            .iter()
            .zip(&other.cells)
            .map(|(mine, theirs)| {
                let mut both = mine.clone();
                for (&family, &n) in theirs {
                    *both.entry(family).or_default() += n;
                }
                let total: u64 = both.values().sum();
                total - both.values().copied().max().unwrap_or(0)
            })
            .sum()
    }
    /// The call at each count of points.
    fn profile(&self) -> Vec<Family> {
        self.cells.iter().map(Group::winner).collect()
    }
}

/// How many counts of points two profiles disagree about.
fn conflicts(a: &[Family], b: &[Family]) -> usize {
    a.iter().zip(b).filter(|(a, b)| a != b).count()
}

/// The table of one page.
pub(super) struct Blocks {
    /// The count of points of the first column.
    first: usize,
    /// The family of each cell of each row, top to bottom.
    rows: Vec<Vec<Family>>,
    /// The row of each kind of hand.
    #[cfg_attr(not(test), allow(dead_code))]
    row_of: HashMap<Kind, usize>,
}

impl Blocks {
    /// The table of `page`, or `None` if no hands reached it.
    pub(super) fn of(page: &Page) -> Option<Blocks> {
        let total: u64 = page.visits.iter().map(|&v| v as u64).sum();
        // Rare counts of points are left off the ends.
        let shown = |p: usize| 1000 * page.visits[p] as u64 >= total;
        let first = (0..POINTS).find(|&p| shown(p))?;
        let last = (0..POINTS).rev().find(|&p| shown(p))?;

        // What the robot does with each kind of hand at each count of points
        // comes from its table, by trying a hand of the kind there, so that
        // a count that is rare for the kind still has its call.  How common
        // the kind is, from the samples, says which kinds matter most.
        let families = Families::new(page);
        let mut mass: HashMap<Kind, u64> = HashMap::new();
        for (&(kind, _, _), &n) in page.samples.iter() {
            *mass.entry(kind).or_default() += n as u64;
        }
        let mut kinds: Vec<(Kind, Group)> = mass
            .into_iter()
            .map(|(kind, mass)| {
                let hand = &page.representatives[&kind];
                let cells = (0..POINTS)
                    .map(|p| {
                        let call = page.choose(&with_points(hand, p));
                        Cell::from([(families.of(call), mass)])
                    })
                    .collect();
                (kind, Group { mass, cells })
            })
            .collect();
        kinds.sort_by_key(|(kind, group)| (Reverse(group.mass), *kind));

        // Kinds of hand that the robot treats alike share a row.
        let mut rows: Vec<(Vec<Kind>, Group)> = Vec::new();
        for (kind, group) in kinds {
            let profile = group.profile();
            match rows
                .iter_mut()
                .find(|(_, row)| conflicts(&row.profile(), &profile) == 0)
            {
                Some((members, row)) => {
                    members.push(kind);
                    row.merge(group);
                }
                None => rows.push((vec![kind], group)),
            }
        }
        // Too many rows are not a table.  A rare kind of hand goes to the row
        // that loses least by it while there are many rows, and then the two
        // rows whose join disagrees with the fewest hands are joined.  What
        // they disagree about goes to the call with more hands.
        const MANY: usize = 40;
        while rows.len() > MANY {
            let (members, group) = rows.pop().unwrap();
            let target = (0..rows.len())
                .min_by_key(|&i| (rows[i].1.loss(&group), i))
                .unwrap();
            let into = &mut rows[target];
            into.0.extend(members);
            into.1.merge(group);
            rows.sort_by_key(|(members, group)| (Reverse(group.mass), members[0]));
        }
        while rows.len() > MAX_ROWS {
            let (i, j) = (0..rows.len())
                .flat_map(|i| (i + 1..rows.len()).map(move |j| (i, j)))
                .min_by_key(|&(i, j)| (rows[i].1.loss(&rows[j].1), i, j))
                .unwrap();
            let (members, group) = rows.remove(j);
            rows[i].0.extend(members);
            rows[i].1.merge(group);
        }

        // Rows that are alike go together, so that blocks reach across them.
        let mut ordered: Vec<(Vec<Kind>, Group)> = Vec::new();
        while !rows.is_empty() {
            let next = match ordered.last() {
                None => 0,
                Some((_, last)) => {
                    let last = last.profile();
                    (0..rows.len())
                        .min_by_key(|&i| (conflicts(&last, &rows[i].1.profile()), i))
                        .unwrap()
                }
            };
            ordered.push(rows.remove(next));
        }

        let mut row_of = HashMap::new();
        let mut table = Vec::new();
        for (r, (members, group)) in ordered.iter().enumerate() {
            row_of.extend(members.iter().map(|&kind| (kind, r)));
            let profile = group.profile();
            table.push(profile[first..=last].to_vec());
        }
        Some(Blocks {
            first,
            rows: table,
            row_of,
        })
    }

    /// How many of the calls in `page.samples` that fall in the table are the
    /// call that the table gives them.
    #[cfg(test)]
    pub(super) fn agreement(&self, page: &Page) -> f64 {
        let families = Families::new(page);
        let (mut agree, mut all) = (0u64, 0u64);
        for (&(kind, place, call), &n) in page.samples.iter() {
            let place = place as usize;
            if place < self.first || place >= self.first + self.rows[0].len() {
                continue;
            }
            all += n as u64;
            if self.rows[self.row_of[&kind]][place - self.first] == families.of(call) {
                agree += n as u64;
            }
        }
        agree as f64 / all.max(1) as f64
    }
}

/// How wide a count of points is, and how tall a row.
const CELL_W: usize = 14;
const ROW_H: usize = 30;
/// Room above the table for the counts of points.
const TOP: usize = 16;
/// The font size of a label set small, and how wide one of its characters is.
const SMALL_PX: f64 = 9.0;
const SMALL_EM: f64 = 5.6;

impl Blocks {
    /// The blocks of the table, packed into as few units of height as they go
    /// in.
    fn layout(&self) -> Layout {
        Layout::of(&self.rows)
    }
}

impl Booklet {
    /// The table of blocks of `page` as an SVG picture, or nothing if no hands
    /// reached the page.  Each block links to the page of partner's reply to
    /// its commonest call, and says what the system means by its calls when
    /// the mouse is over it.
    pub(super) fn block_table(&self, page: &Page) -> String {
        let blocks = match Blocks::of(page) {
            Some(blocks) => blocks,
            None => return String::new(),
        };
        let layout = blocks.layout();
        let (height, width) = (layout.height, layout.width);
        let of = layout.grid();
        let lines = self.lines(page);
        let families = Families::new(page);

        let mut out = String::new();
        out.push_str(&format!(
            "<svg class=\"blocks\" viewBox=\"0 0 {w} {h}\" width=\"{w}\" height=\"{h}\" \
             role=\"img\" aria-label=\"Which call the robot makes, by points and kind of hand\">\n",
            w = width * CELL_W,
            h = TOP + height * ROW_H,
        ));
        for c in 0..width {
            let points = blocks.first + c;
            if points % 10 == 0 {
                out.push_str(&format!(
                    "<text class=\"tick\" x=\"{}\" y=\"{}\">{}</text>\n",
                    c * CELL_W + CELL_W / 2,
                    TOP / 2,
                    points
                ));
            }
        }
        for block in layout.blocks.iter() {
            let mut calls: Vec<&super::Line> = {
                let mut calls: Vec<&super::Line> = lines
                    .iter()
                    .filter(|l| l.made > 0 && families.of(l.call) == block.family)
                    .collect();
                calls.sort_by_key(|l| label_order(l.call));
                calls
            };
            // A block of the table can be one the robot never made in the
            // sample, so name it by the calls the system has in its family,
            // and failing those by the family.
            if calls.is_empty() {
                calls = lines
                    .iter()
                    .filter(|l| families.of(l.call) == block.family)
                    .collect();
                calls.sort_by_key(|l| label_order(l.call));
            }
            let (mut name, mut plain_name) = names(&calls);
            if calls.is_empty() {
                name = escape(&families.names[block.family.0]);
                plain_name = families.names[block.family.0].clone();
            }
            let (lo, hi) = (block.first, block.last());
            let range = if lo == hi {
                format!("{}", blocks.first + lo)
            } else {
                format!("{}–{}", blocks.first + lo, blocks.first + hi)
            };
            let title = std::iter::once(format!(
                "{}, {} points",
                families.names[block.family.0], range
            ))
            .chain(calls.iter()
                .map(|l| {
                    let meaning = l.meaning.map_or(
                        "Nothing else fits, so the robot passes.".to_string(),
                        |m| m.to_string(),
                    );
                    format!("{}: {}", super::call(l.call), meaning)
                }))
                .collect::<Vec<_>>()
                .join("\n");
            let reply = calls
                .iter()
                .max_by_key(|l| (l.made, Reverse(label_order(l.call))))
                .and_then(|l| l.reply.first().copied());

            // The block as one path: a rectangle for each run of columns in
            // which it has the same top and bottom.
            let mut path = String::new();
            let mut c = 0;
            while c < block.bands.len() {
                let (top, bottom) = block.bands[c];
                let mut length = 1;
                while block.bands.get(c + length) == Some(&(top, bottom)) {
                    length += 1;
                }
                path.push_str(&format!(
                    "M{} {}h{}v{}h-{}z",
                    (block.first + c) * CELL_W,
                    TOP + top * ROW_H,
                    length * CELL_W,
                    (bottom - top) * ROW_H,
                    length * CELL_W
                ));
                c += length;
            }
            // The label goes in the widest rectangle of the block.  Where the
            // name does not fit along it, it is set smaller, and where it
            // still does not, it is turned to run up the tallest rectangle.
            let (c, length, top, bottom) = block.label_room();
            let x = (c * CELL_W) as f64 + (length * CELL_W) as f64 / 2.0;
            let y = TOP as f64 + ((top + bottom) * ROW_H) as f64 / 2.0;
            let room = (length * CELL_W) as f64;
            let fits = |room: f64, chars: usize, em: f64| room >= chars as f64 * em + 6.0;
            if !calls.is_empty() && !fits(room, plain_name.chars().count(), 7.5) {
                (name, plain_name) = short_names(&calls);
            }
            let chars = plain_name.chars().count();
            // The second line says what the calls are, if there is room, and
            // otherwise at which points.
            let kind = families.names[block.family.0].as_str();
            let second = if kind != plain_name && fits(room, kind.chars().count(), 5.8) {
                Some(escape(kind))
            } else if fits(room, range.chars().count(), 5.8) {
                Some(range.clone())
            } else {
                None
            };
            let label = if fits(room, chars, 7.5) {
                match second {
                    Some(second) => format!(
                        "<text x=\"{x}\" y=\"{}\">{}</text><text class=\"range\" x=\"{x}\" y=\"{}\">{}</text>",
                        y - 6.0,
                        name,
                        y + 8.0,
                        second
                    ),
                    None => format!("<text x=\"{}\" y=\"{}\">{}</text>", x, y, name),
                }
            } else if fits(room, chars, SMALL_EM) {
                format!("<text class=\"small\" x=\"{}\" y=\"{}\">{}</text>", x, y, name)
            } else {
                let (c, length, top, bottom) = block.tall_room();
                let x = (c * CELL_W) as f64 + (length * CELL_W) as f64 / 2.0;
                let y = TOP as f64 + ((top + bottom) * ROW_H) as f64 / 2.0;
                let across = (length * CELL_W) as f64;
                let along = ((bottom - top) * ROW_H) as f64;
                if across >= SMALL_PX + 3.0 && fits(along, chars, SMALL_EM) {
                    format!(
                        "<text class=\"small\" transform=\"rotate(-90 {x} {y})\" x=\"{x}\" y=\"{y}\">{}</text>",
                        name
                    )
                } else {
                    // The calls one above another, each as short as it goes.
                    let widest = calls
                        .iter()
                        .map(|l| super::call(l.call).chars().count())
                        .max()
                        .unwrap_or(0);
                    let line = SMALL_PX + 1.0;
                    if !calls.is_empty() && fits(across, widest, SMALL_EM) && along >= calls.len() as f64 * line + 2.0 {
                        let first = y - (calls.len() as f64 - 1.0) * line / 2.0;
                        calls
                            .iter()
                            .enumerate()
                            .map(|(i, l)| {
                                format!(
                                    "<text class=\"small\" x=\"{}\" y=\"{}\">{}</text>",
                                    x,
                                    first + i as f64 * line,
                                    call_svg(l.call)
                                )
                            })
                            .collect()
                    } else {
                        // Only the first call, and dots for the others.
                        let short = format!("{}…", call_svg(calls[0].call));
                        let chars = super::call(calls[0].call).chars().count() + 1;
                        if calls.is_empty() {
                            String::new()
                        } else if fits(across, chars, SMALL_EM) {
                            format!("<text class=\"small\" x=\"{}\" y=\"{}\">{}</text>", x, y, short)
                        } else if across >= SMALL_PX + 3.0 && fits(along, chars, SMALL_EM) {
                            format!(
                                "<text class=\"small\" transform=\"rotate(-90 {x} {y})\" x=\"{x}\" y=\"{y}\">{}</text>",
                                short
                            )
                        } else {
                            String::new()
                        }
                    }
                }
            };

            let link = reply.map(|p| format!("<a href=\"#p{}\">", p + 1));
            out.push_str(&format!(
                "{}<g class=\"{}\"><title>{}</title><path d=\"{}\"/>{}",
                link.as_deref().unwrap_or(""),
                families.classes[block.family.0],
                escape(&title),
                path,
                label
            ));
            out.push_str("</g>");
            if link.is_some() {
                out.push_str("</a>");
            }
            out.push('\n');
        }

        // Thin lines where one block ends and another begins.
        let mut edges = String::new();
        for r in 1..height {
            let mut c = 0;
            while c < width {
                if of[r - 1][c] != of[r][c] {
                    let start = c;
                    while c < width && of[r - 1][c] != of[r][c] {
                        c += 1;
                    }
                    edges.push_str(&format!(
                        "M{} {}h{}",
                        start * CELL_W,
                        TOP + r * ROW_H,
                        (c - start) * CELL_W
                    ));
                } else {
                    c += 1;
                }
            }
        }
        for c in 1..width {
            let mut r = 0;
            while r < height {
                if of[r][c - 1] != of[r][c] {
                    let start = r;
                    while r < height && of[r][c - 1] != of[r][c] {
                        r += 1;
                    }
                    edges.push_str(&format!(
                        "M{} {}v{}",
                        c * CELL_W,
                        TOP + start * ROW_H,
                        (r - start) * ROW_H
                    ));
                } else {
                    r += 1;
                }
            }
        }
        out.push_str(&format!("<path class=\"edge\" d=\"{}\"/>\n</svg>\n", edges));
        out
    }
}

#[cfg(test)]
impl Blocks {
    /// Has every row a call for every count of points, and are there at most
    /// `MAX_ROWS`?
    pub(super) fn rows_are_complete(&self) -> bool {
        !self.rows.is_empty()
            && self.rows.len() <= MAX_ROWS
            && self.rows.iter().all(|row| row.len() == self.rows[0].len())
    }
}
