//! A sketch of a booklet that explains a bidding system the way a
//! choose-your-own-adventure book does.  Each page starts from an auction and
//! lists the calls that may come next, most likely first, each with the page
//! that follows it.
//!
//! A page holds every auction after which the system gives each call the same
//! meaning, so one page covers an opening in any seat, or a convention that is
//! reached in several ways.  Which auctions come up, and how often each call is
//! made, come from letting the robot bid seeded random deals.

#[cfg(test)]
use super::ShapeRule;
use super::{choose, fitting_calls, seeded_hands, Convention, Described, Fit, Forcing};
use crate::utils::plain;
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
#[cfg(test)]
use display_as::{format_as, HTML};
mod html;

use std::collections::HashMap;
use std::fmt;

/// A sketch of the booklet for one bidding system: its pages, with the
/// auctions each covers, the calls that may come next and where they lead.
pub struct Booklet {
    deals: u64,
    min_reached: u64,
    auctions: HashMap<Vec<Bid>, Auction>,
    /// The pages, in reading order.
    pages: Vec<Page>,
    /// The page of each auction that has one.
    page_of: HashMap<Vec<Bid>, usize>,
}

/// An auction the robot reached: what the system understands next, how often
/// the robot got there and what it bid.
struct Auction {
    /// The legal calls the system understands, highest first.
    described: Vec<Described>,
    reached: u64,
    calls: HashMap<Bid, u64>,
    /// How often the hand fit each call, whether or not it was made.
    fitted: HashMap<Bid, u64>,
}

/// One entry of the table that describes a call after some auction, which is
/// a step in the robot's choice: it makes the first call, going down the
/// entries in table order, that has an entry some way of which fits its hand.
#[derive(Clone, PartialEq, Eq, Hash)]
struct Decision {
    call: Bid,
    name: String,
    forcing: Forcing,
    /// The ways a hand can fit: bounds on its values, and rules about its
    /// shape.
    fits: Vec<Fit>,
    /// The summary of the entry that the game shows when the mouse is over
    /// the call, as HTML.
    description: String,
}

impl Decision {
    /// Does `hand` fit in any of the ways?
    #[cfg(test)]
    fn holds(&self, hand: &HandValuation) -> bool {
        self.fits.iter().any(|f| f.holds(hand))
    }
}

/// The ways a hand can fit `convention`.
fn fits_of(convention: &Convention) -> Vec<Fit> {
    match convention {
        Convention::Simple { min, max, .. }
        | Convention::Natural { min, max, .. }
        | Convention::Forced { min, max, .. } => vec![Fit::new(*min, *max)],
        Convention::Rule { fits, .. } => fits.clone(),
        Convention::Either(options) => options.iter().flat_map(fits_of).collect(),
        Convention::Ordered { .. } => Vec::new(),
    }
}

/// The steps of the robot's choice among `described`: every entry that
/// describes a call, in table order, and the higher call first when it is the
/// same entry.  Two auctions after which these are equal are bid alike by the
/// robot, whatever the hand.
fn decisions(described: &[Described]) -> Vec<Decision> {
    let mut steps: Vec<(usize, usize, Decision)> = Vec::new();
    for (position, d) in described.iter().enumerate() {
        for (index, entry) in d.entries() {
            steps.push((
                *index,
                position,
                Decision {
                    call: d.bid,
                    name: entry.name().into_string(),
                    forcing: entry.is_forcing(),
                    fits: fits_of(entry),
                    description: entry.description().into_string(),
                },
            ));
        }
    }
    steps.sort_by_key(|&(index, position, _)| (index, position));
    steps.into_iter().map(|(_, _, d)| d).collect()
}

/// What a call means after some auction, as the sketch shows it: how forcing
/// it is, and the summary that the game shows when the mouse is over the call.
#[derive(Clone)]
struct Meaning {
    call: Bid,
    forcing: Forcing,
    /// The summary as HTML.  When several entries describe the call it has
    /// the summary of each, as the game shows them.
    description: String,
}

/// The meaning of each call among `decisions`, in the order the robot first
/// considers them.
fn meanings(decisions: &[Decision]) -> Vec<Meaning> {
    let mut calls: Vec<Bid> = Vec::new();
    for d in decisions {
        if !calls.contains(&d.call) {
            calls.push(d.call);
        }
    }
    calls
        .into_iter()
        .map(|call| {
            let of_call: Vec<&Decision> = decisions.iter().filter(|d| d.call == call).collect();
            Meaning {
                call,
                forcing: of_call.iter().map(|d| d.forcing).max().unwrap(),
                description: of_call
                    .iter()
                    .map(|d| d.description.as_str())
                    .collect::<Vec<_>>()
                    .join(super::OR),
            }
        })
        .collect()
}

/// A summary as one line of plain text, with the parts separated by
/// semicolons.
fn one_line(html: &str) -> String {
    // A marker, which `plain` leaves alone, stands for the "or" between the
    // summaries of different entries.
    let marked = html
        .replace(super::OR, "<br/>\u{0}<br/>")
        .replace("<br/>", "\n");
    let mut line = String::new();
    for part in plain(&marked)
        .lines()
        .map(str::trim)
        .filter(|p| !p.is_empty())
    {
        if part == "\u{0}" {
            line.push_str(" or ");
        } else {
            if !line.is_empty() && !line.ends_with(" or ") {
                line.push_str("; ");
            }
            line.push_str(part);
        }
    }
    line
}

/// How many different counts of points the bars show: 0 to 40.
const POINTS: usize = 41;

/// The way of counting points that a page's bars are drawn along.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Points {
    Hcp,
    Lhcp,
    Shcp,
}

impl Points {
    /// The count of these points in `hand`, as a place along a bar.
    fn of(self, hand: &HandValuation) -> usize {
        let points = match self {
            Points::Hcp => hand.hcp,
            Points::Lhcp => hand.lhcp,
            Points::Shcp => hand.shcp,
        };
        (points as usize).min(POINTS - 1)
    }
    fn name(self) -> &'static str {
        match self {
            Points::Hcp => "high card points",
            Points::Lhcp => "long points (high card points and length)",
            Points::Shcp => "short points (high card points and shortness)",
        }
    }
}

/// The points that the calls made most often bound, so that the bars say as
/// much as they can: a call's bar is blocked out where its bounds on those
/// points allow it.  `made` is how often each call was made.  High card points
/// when none is clearly more used.
fn points_axis(decisions: &[Decision], made: &HashMap<Bid, u64>) -> Points {
    let bounded = |points: Points| -> u64 {
        let of = |hand: &HandValuation| match points {
            Points::Hcp => hand.hcp,
            Points::Lhcp => hand.lhcp,
            Points::Shcp => hand.shcp,
        };
        decisions
            .iter()
            .filter(|d| {
                d.fits.iter().any(|f| {
                    of(&f.min) > of(&HandValuation::MIN) || of(&f.max) < of(&HandValuation::MAX)
                })
            })
            .map(|d| made.get(&d.call).copied().unwrap_or(0))
            .sum()
    };
    let mut best = Points::Hcp;
    for points in [Points::Lhcp, Points::Shcp] {
        if bounded(points) > bounded(best) {
            best = points;
        }
    }
    best
}

/// How often each call was made after the auctions of a page.
fn made_on_page(members: &[Vec<Bid>], auctions: &HashMap<Vec<Bid>, Auction>) -> HashMap<Bid, u64> {
    let mut made: HashMap<Bid, u64> = HashMap::new();
    for bids in members {
        for (&call, &times) in auctions[bids].calls.iter() {
            *made.entry(call).or_default() += times;
        }
    }
    made
}

struct Page {
    /// The auctions this page covers, most often reached first.
    auctions: Vec<Vec<Bid>>,
    /// How the robot chooses a call after any of them.
    decisions: Vec<Decision>,
    /// The calls the system understands after them, in the robot's order.
    table: Vec<Meaning>,
    /// The points that the bars are drawn along.
    points: Points,
    /// How often each count of those points reached the page.
    visits: [u32; POINTS],
    /// How often each call was made with each count.
    made: HashMap<Bid, [u32; POINTS]>,
}

impl Page {
    /// The call the robot makes with `hand`, going only by this page.
    #[cfg(test)]
    fn choose(&self, hand: &HandValuation) -> Bid {
        self.decisions
            .iter()
            .find(|d| d.holds(hand))
            .map_or(Bid::Pass, |d| d.call)
    }
}

/// One call on a page: how often it was made, what it means, and the pages
/// that follow it, most often reached first.
struct Line<'a> {
    call: Bid,
    meaning: Option<&'a Meaning>,
    made: u64,
    /// The page of the next bidder, an opponent, unless they almost always
    /// pass.
    next: Vec<usize>,
    /// The page of partner's reply, when the opponent passes.
    reply: Vec<usize>,
}

impl Booklet {
    /// Let the robot bid `deals` seeded deals by `convention`, and give a page
    /// to every auction it reached at least `min_reached` times.
    pub fn new(convention: &Convention, deals: u64, min_reached: u64) -> Self {
        let mut auctions: HashMap<Vec<Bid>, Auction> = HashMap::new();
        // The whole auction of each deal, to count by points once the pages
        // are known without bidding again.
        let mut deal_auctions: Vec<Vec<Bid>> = Vec::with_capacity(deals as usize);
        for seed in 0..deals {
            let hands = seeded_hands(seed);
            let mut bids = Vec::new();
            while !auction_over(&bids) {
                let auction = auctions.entry(bids.clone()).or_insert_with(|| Auction {
                    described: convention.described_bids(&bids),
                    reached: 0,
                    calls: HashMap::new(),
                    fitted: HashMap::new(),
                });
                let hand = hands[bids.len() % 4].values();
                let fitting = fitting_calls(&auction.described, &hand);
                let call = choose(&fitting).unwrap_or(Bid::Pass);
                for &(_, fits) in fitting.iter() {
                    *auction.fitted.entry(fits).or_default() += 1;
                }
                auction.reached += 1;
                *auction.calls.entry(call).or_default() += 1;
                bids.push(call);
            }
            deal_auctions.push(bids);
        }
        auctions.retain(|_, a| a.reached >= min_reached);

        let mut by_decisions: HashMap<Vec<Decision>, Vec<Vec<Bid>>> = HashMap::new();
        for (bids, auction) in auctions.iter() {
            by_decisions
                .entry(decisions(&auction.described))
                .or_default()
                .push(bids.clone());
        }
        let mut booklet = Booklet {
            deals,
            min_reached,
            pages: Vec::new(),
            page_of: HashMap::new(),
            auctions,
        };
        // The hash maps list things in an order that changes from run to run,
        // so everything is put in an order of its own before it gets a number:
        // the auctions of a page, most often reached first and then the
        // shorter and lower ones; and the pages the same way.
        let mut pages: Vec<(u64, Vec<Vec<Bid>>, Vec<Decision>)> = Vec::new();
        for (decisions, mut members) in by_decisions {
            members.sort_by_key(|bids| {
                (
                    std::cmp::Reverse(booklet.auctions[bids].reached),
                    auction_key(bids),
                )
            });
            let reached = members.iter().map(|b| booklet.auctions[b].reached).sum();
            pages.push((reached, members, decisions));
        }
        pages.sort_by_key(|(reached, members, _)| {
            (std::cmp::Reverse(*reached), auction_key(&members[0]))
        });
        for (_, members, decisions) in pages {
            let points = points_axis(&decisions, &made_on_page(&members, &booklet.auctions));
            booklet.pages.push(Page {
                auctions: members,
                table: meanings(&decisions),
                points,
                visits: [0; POINTS],
                made: HashMap::new(),
                decisions,
            });
        }
        booklet.put_in_reading_order();
        booklet.count_by_points(&deal_auctions);
        booklet
    }

    /// Go through the deals again, with the auction of each, to count for
    /// each page how often each call was made with each count of points.
    /// The pages are known by now, so nothing is kept for the auctions that
    /// have none.
    fn count_by_points(&mut self, deal_auctions: &[Vec<Bid>]) {
        for (seed, bids) in deal_auctions.iter().enumerate() {
            let hands = seeded_hands(seed as u64);
            for (i, &call) in bids.iter().enumerate() {
                if let Some(&page) = self.page_of.get(&bids[..i]) {
                    let hand = hands[i % 4].values();
                    let page = &mut self.pages[page];
                    let place = page.points.of(&hand);
                    page.visits[place] += 1;
                    page.made.entry(call).or_insert([0; POINTS])[place] += 1;
                }
            }
        }
    }

    /// A short number for the tables, so that a booklet made from the same
    /// kind of tables can be told from one made from changed tables: the
    /// sketch of a small booklet, boiled down.
    pub fn fingerprint(convention: &Convention) -> String {
        let sketch = Booklet::new(convention, 300, 3).to_string();
        let hash = sketch.bytes().fold(0xcbf29ce484222325u64, |hash, byte| {
            (hash ^ byte as u64).wrapping_mul(0x100000001b3)
        });
        format!("{:016x}", hash)
    }

    /// Number the pages from the opening, each followed by the pages its calls
    /// lead to: the most likely call first, and partner's reply to a call
    /// before what the opponents might do instead.
    fn put_in_reading_order(&mut self) {
        self.number_pages();
        let mut numbered = vec![false; self.pages.len()];
        let mut order = Vec::new();
        // With too few deals the opening may not have a page, and then there
        // are no pages to put in order.
        let mut stack: Vec<usize> = self.page_of.get(&Vec::new()).copied().into_iter().collect();
        while let Some(page) = stack.pop() {
            if std::mem::replace(&mut numbered[page], true) {
                continue;
            }
            order.push(page);
            for line in self.lines(&self.pages[page]).iter().rev() {
                stack.extend(line.next.iter().rev());
                stack.extend(line.reply.iter().rev());
            }
        }
        // Pages that no call leads to, if any, go at the end.
        order.extend((0..self.pages.len()).filter(|&p| !numbered[p]));
        let mut pages: Vec<Option<Page>> = self.pages.drain(..).map(Some).collect();
        self.pages = order.iter().map(|&p| pages[p].take().unwrap()).collect();
        self.number_pages();
    }

    /// Record the page of every auction.
    fn number_pages(&mut self) {
        for (n, page) in self.pages.iter().enumerate() {
            for bids in page.auctions.iter() {
                self.page_of.insert(bids.clone(), n);
            }
        }
    }

    /// Do the players on `page` almost always pass, so that it is not worth
    /// turning to?  They do if fewer than 1 in 100 of their calls are not
    /// Pass.
    fn almost_always_passes(&self, page: usize) -> bool {
        let page = &self.pages[page];
        let passes: u64 = page
            .auctions
            .iter()
            .filter_map(|bids| self.auctions[bids].calls.get(&Bid::Pass))
            .sum();
        100 * (self.reached(page) - passes) < self.reached(page)
    }

    /// How often the robot reached the auctions of `page`.
    fn reached(&self, page: &Page) -> u64 {
        page.auctions.iter().map(|b| self.auctions[b].reached).sum()
    }

    /// The calls of `page`, most often made first, then those the system
    /// understands but the robot never made, in the robot's order.
    fn lines<'a>(&self, page: &'a Page) -> Vec<Line<'a>> {
        let mut lines: Vec<Line> = page
            .table
            .iter()
            .map(|m| (m.call, Some(m)))
            .chain(std::iter::once((Bid::Pass, None)))
            .filter(|(call, meaning)| {
                meaning.is_some() || page.table.iter().all(|m| m.call != *call)
            })
            .map(|(call, meaning)| Line {
                call,
                meaning,
                made: 0,
                next: self
                    .pages_after(&page.auctions, &[call])
                    .into_iter()
                    .filter(|&p| !self.almost_always_passes(p))
                    .collect(),
                reply: self.pages_after(&page.auctions, &[call, Bid::Pass]),
            })
            .collect();
        for line in lines.iter_mut() {
            line.made = page
                .auctions
                .iter()
                .filter_map(|bids| self.auctions[bids].calls.get(&line.call))
                .sum();
        }
        lines.retain(|line| line.meaning.is_some() || line.made > 0);
        lines.sort_by_key(|line| std::cmp::Reverse(line.made));
        lines
    }

    /// Things in the sketch that look wrong in the tables, for someone who is
    /// reviewing them, one to a line.  Each is about a page that was reached
    /// often enough to say, and none is certain: it is a place to look.
    ///
    /// - A forcing call that partner then passes (when the opponents do) in at
    ///   least 1 time in 20.
    /// - A game contract, which is 3NT, 4♥, 4♠, 5♣ or 5♦, that hands often
    ///   fit but the robot never bids, because something that comes earlier
    ///   in the table always wins.
    pub fn anomalies(&self) -> Vec<String> {
        const GAMES: [Bid; 5] = [
            Bid::NT(3),
            Bid::Suit(4, Suit::Hearts),
            Bid::Suit(4, Suit::Spades),
            Bid::Suit(5, Suit::Clubs),
            Bid::Suit(5, Suit::Diamonds),
        ];
        let mut found = Vec::new();
        for (n, page) in self.pages.iter().enumerate() {
            let reached = self.reached(page);
            for line in self.lines(page) {
                let Some(meaning) = line.meaning else {
                    continue;
                };
                if matches!(meaning.forcing, Forcing::Forcing | Forcing::GameForcing) {
                    let (mut visits, mut passes) = (0, 0);
                    for bids in page.auctions.iter() {
                        let mut reply = bids.clone();
                        reply.extend([line.call, Bid::Pass]);
                        if let Some(a) = self.auctions.get(&reply) {
                            visits += a.reached;
                            passes += a.calls.get(&Bid::Pass).copied().unwrap_or(0);
                        }
                    }
                    if visits >= 10 && 20 * passes >= visits {
                        found.push(format!(
                            "Page {}: {} is {} but partner then passes {} of the {} times \
                             they reply (page {}).",
                            n + 1,
                            call(line.call),
                            if meaning.forcing == Forcing::Forcing {
                                "forcing"
                            } else {
                                "game forcing"
                            },
                            passes,
                            visits,
                            page_numbers(&line.reply),
                        ));
                    }
                }
                if line.made == 0 && GAMES.contains(&line.call) {
                    let fitted: u64 = page
                        .auctions
                        .iter()
                        .filter_map(|bids| self.auctions[bids].fitted.get(&line.call))
                        .sum();
                    if fitted >= 10 && 50 * fitted >= reached {
                        found.push(format!(
                            "Page {}: hands fit {} in {} of {} visits, but the robot never \
                             bids it.",
                            n + 1,
                            call(line.call),
                            fitted,
                            reached,
                        ));
                    }
                }
            }
        }
        found
    }

    /// The pages of `auctions` continued by `calls`, most often reached first.
    fn pages_after(&self, auctions: &[Vec<Bid>], calls: &[Bid]) -> Vec<usize> {
        let mut reached: HashMap<usize, u64> = HashMap::new();
        for bids in auctions {
            let next: Vec<Bid> = bids.iter().chain(calls).copied().collect();
            if let Some(&page) = self.page_of.get(&next) {
                *reached.entry(page).or_default() += self.auctions[&next].reached;
            }
        }
        let mut pages: Vec<(usize, u64)> = reached.into_iter().collect();
        pages.sort_by_key(|&(page, reached)| (std::cmp::Reverse(reached), page));
        pages.into_iter().map(|(page, _)| page).collect()
    }
}

/// A way of putting auctions in an order: the shorter first, then call by
/// call, lowest first.
fn auction_key(bids: &[Bid]) -> (usize, Vec<(u8, usize, u8)>) {
    let call = |b: &Bid| match *b {
        Bid::Pass => (0, 0, 0),
        Bid::Double => (1, 0, 0),
        Bid::Redouble => (2, 0, 0),
        Bid::Suit(level, suit) => (3, level, suit as u8),
        Bid::NT(level) => (3, level, 4),
    };
    (bids.len(), bids.iter().map(call).collect())
}

/// Has everyone had their say?
fn auction_over(bids: &[Bid]) -> bool {
    bids.len() > 3 && bids[bids.len() - 3..] == [Bid::Pass, Bid::Pass, Bid::Pass]
}

/// A call as the sketch shows it, such as "1♥", "2NT", "X" or "Pass".
fn call(bid: Bid) -> String {
    match bid {
        Bid::Pass => "Pass".to_string(),
        Bid::Double => "X".to_string(),
        Bid::Redouble => "XX".to_string(),
        Bid::Suit(level, suit) => format!("{}{}", level, suit.unicode()),
        Bid::NT(level) => format!("{}NT", level),
    }
}

/// An auction as the sketch shows it, such as "P 1♥ P".
fn auction(bids: &[Bid]) -> String {
    if bids.is_empty() {
        return "(no calls yet)".to_string();
    }
    let calls: Vec<String> = bids
        .iter()
        .map(|&b| {
            if b == Bid::Pass {
                "P".to_string()
            } else {
                call(b)
            }
        })
        .collect();
    calls.join(" ")
}

/// Pages as the sketch shows them, such as "p3" or "p3/p17", with at most
/// three of them.
fn page_numbers(pages: &[usize]) -> String {
    let mut numbers: Vec<String> = pages
        .iter()
        .take(3)
        .map(|p| format!("p{}", p + 1))
        .collect();
    if pages.len() > 3 {
        numbers.push("…".to_string());
    }
    numbers.join("/")
}

impl fmt::Display for Meaning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&one_line(&self.description))
    }
}

impl fmt::Display for Booklet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "{} pages for the {} auctions reached at least {} times in {} deals.",
            self.pages.len(),
            self.auctions.len(),
            self.min_reached,
            self.deals,
        )?;
        const SHOWN: usize = 4;
        for (n, page) in self.pages.iter().enumerate() {
            let reached = self.reached(page);
            writeln!(f)?;
            writeln!(
                f,
                "Page {}: reached {} times, after {} auction{}",
                n + 1,
                reached,
                page.auctions.len(),
                if page.auctions.len() == 1 { "" } else { "s" },
            )?;
            let mut shown: Vec<String> = page
                .auctions
                .iter()
                .take(SHOWN)
                .map(|b| auction(b))
                .collect();
            if page.auctions.len() > SHOWN {
                shown.push(format!("and {} more", page.auctions.len() - SHOWN));
            }
            writeln!(f, "  {}", shown.join(" · "))?;
            let mut never = Vec::new();
            for line in self.lines(page) {
                if line.made == 0 {
                    never.push(call(line.call));
                    continue;
                }
                write!(
                    f,
                    "  {:>5.1}%  {:<5} {}",
                    100.0 * line.made as f64 / reached as f64,
                    call(line.call),
                    line.meaning
                        .map_or("(nothing fits, so the robot passes)".to_string(), |m| m
                            .to_string()),
                )?;
                if !line.reply.is_empty() {
                    write!(f, "  → {}", page_numbers(&line.reply))?;
                }
                if !line.next.is_empty() {
                    write!(f, " (they may bid: {})", page_numbers(&line.next))?;
                }
                writeln!(f)?;
            }
            if !never.is_empty() {
                writeln!(f, "  Understood but never made: {}", never.join(" "))?;
            }
        }
        Ok(())
    }
}

/// A booklet from few deals, which the tests share.
#[cfg(test)]
fn small_booklet() -> &'static Booklet {
    static BOOKLET: std::sync::OnceLock<Booklet> = std::sync::OnceLock::new();
    BOOKLET.get_or_init(|| Booklet::new(&Convention::sayc(), 200, 4))
}

#[test]
fn the_first_page_is_the_opening_in_the_first_three_seats() {
    let booklet = small_booklet();
    let mut first = booklet.pages[0].auctions.clone();
    first.sort_by_key(|bids| bids.len());
    assert_eq!(
        first,
        vec![vec![], vec![Bid::Pass], vec![Bid::Pass, Bid::Pass]]
    );
}

#[test]
fn every_visit_to_a_page_makes_one_call() {
    let booklet = small_booklet();
    for page in booklet.pages.iter() {
        let made: u64 = booklet.lines(page).iter().map(|line| line.made).sum();
        assert_eq!(made, booklet.reached(page), "{:?}", page.auctions);
    }
}

#[test]
fn every_link_continues_the_auctions_of_its_page() {
    let booklet = small_booklet();
    for page in booklet.pages.iter() {
        for line in booklet.lines(page) {
            let links = [
                (&line.next, vec![line.call]),
                (&line.reply, vec![line.call, Bid::Pass]),
            ];
            for (pages, calls) in links {
                for &next in pages.iter() {
                    assert!(page.auctions.iter().any(|bids| {
                        let continued: Vec<Bid> = bids.iter().chain(&calls).copied().collect();
                        booklet.pages[next].auctions.contains(&continued)
                    }));
                }
            }
        }
    }
}

#[test]
fn the_booklet_chooses_the_calls_the_robot_makes() {
    for system in super::BiddingSystem::ALL {
        let convention = system.convention();
        for (_, hands, bids) in super::seeded_deals(system.convention(), 60) {
            for i in 0..bids.len() {
                let described = convention.described_bids(&bids[..i]);
                let hand = hands[i % 4].values();
                assert_eq!(
                    super::robot_choice(&described, &hand).unwrap_or(Bid::Pass),
                    bids[i],
                    "{:?}",
                    &bids[..i]
                );
            }
        }
    }
}

#[test]
fn too_few_deals_make_an_empty_booklet() {
    // The opening is reached once a deal, so none of 3 deals reaches it 4 times.
    let booklet = Booklet::new(&Convention::sayc(), 3, 4);
    assert!(booklet.pages.is_empty());
    assert!(booklet.to_string().starts_with("0 pages"));
    assert!(Booklet::new(&Convention::sayc(), 0, 1).pages.is_empty());
}

/// A table entry for a call, with no bounds, that asks for `rule`.
#[cfg(test)]
fn entry_with(rule: ShapeRule) -> Convention {
    use regex::RegexSet;
    Convention::Rule {
        regex: RegexSet::new(["1S"]).unwrap(),
        the_description: format_as!(HTML, ""),
        the_name: "Opening bid",
        fits: vec![Fit::new(HandValuation::MIN, HandValuation::MAX).with(rule)],
        forcing: Forcing::Passable,
    }
}

#[test]
fn calls_with_different_shape_rules_do_not_choose_alike() {
    use bridge_deck::Suit::{Hearts, Spades};
    let described = |rule: ShapeRule| {
        let entry = entry_with(rule);
        vec![Described::new(Bid::Pass, entry.clone(), vec![(0, entry)])]
    };
    // The hover summary shows the shape rule too, so leave it out to see that
    // the ways of fitting alone tell the decisions apart.
    let without_summary = |mut steps: Vec<Decision>| {
        steps.iter_mut().for_each(|d| d.description.clear());
        steps
    };
    let longer = without_summary(decisions(&described(ShapeRule::Longer(Spades, Hearts))));
    let as_long = without_summary(decisions(&described(ShapeRule::AtLeastAsLong(
        Spades, Hearts,
    ))));
    assert!(longer != as_long);
    assert!(longer == without_summary(decisions(&described(ShapeRule::Longer(Spades, Hearts)))));
}

#[test]
fn calls_whose_entries_interleave_differently_do_not_choose_alike() {
    use bridge_deck::Suit::{Hearts, Spades};
    let rule = ShapeRule::Longer(Spades, Hearts);
    let other = ShapeRule::Longer(Hearts, Spades);
    // Each call has the same ways in the same order in both, but one call's
    // second entry comes before the other call's first in one table and after
    // it in the other, so a hand that fits only those two entries gets a
    // different call.
    let table = |second: usize| {
        let (a, b) = (entry_with(rule), entry_with(other));
        vec![
            Described::new(
                Bid::NT(1),
                a.clone(),
                vec![(0, a.clone()), (second, a.clone())],
            ),
            Described::new(Bid::Pass, b.clone(), vec![(5, b)]),
        ]
    };
    assert!(decisions(&table(3)) != decisions(&table(7)));
}

#[test]
fn the_pages_alone_reproduce_the_robot() {
    // The calls of a page's decisions, with nothing else of the booklet, are
    // the robot's calls after every auction that has a page.
    let mut checked = 0;
    for system in super::BiddingSystem::ALL {
        let convention = system.convention();
        let booklet = Booklet::new(&convention, 300, 3);
        for (_, hands, bids) in super::seeded_deals(system.convention(), 150) {
            for i in 0..bids.len() {
                if let Some(&page) = booklet.page_of.get(&bids[..i]) {
                    let hand = hands[i % 4].values();
                    assert_eq!(
                        booklet.pages[page].choose(&hand),
                        bids[i],
                        "{} after {}",
                        system.name(),
                        auction(&bids[..i])
                    );
                    checked += 1;
                }
            }
        }
    }
    assert!(checked > 500, "only {} calls were checked", checked);
}

/// Prints a booklet for a bidding system, as a sketch to read:
/// `cargo test -p robot --release print_booklet -- --ignored --nocapture`.
/// `BOOKLET_SYSTEM` picks the system by name (SAYC by default) and
/// `BOOKLET_DEALS` the number of deals (100,000, which takes about two
/// minutes).  A page is kept for each auction reached in 1 of 1,000 deals.
#[test]
#[ignore]
fn print_booklet() {
    let name = std::env::var("BOOKLET_SYSTEM").unwrap_or_else(|_| "SAYC".to_string());
    let system = super::BiddingSystem::ALL
        .iter()
        .find(|s| s.name().eq_ignore_ascii_case(&name))
        .unwrap_or_else(|| panic!("no bidding system called {}", name));
    let deals = std::env::var("BOOKLET_DEALS")
        .map_or(100_000, |d| d.parse().expect("BOOKLET_DEALS is a number"));
    let booklet = Booklet::new(&system.convention(), deals, deals / 1000);
    println!("{}", booklet);
    println!("Things to look at in the tables:");
    for anomaly in booklet.anomalies() {
        println!("  {}", anomaly);
    }
}

#[test]
fn the_same_deals_make_the_same_booklet() {
    // Each booklet's hash maps order their entries differently, so the pages
    // and their numbers must not depend on that.
    let first = Booklet::new(&Convention::sayc(), 400, 2).to_string();
    for _ in 0..3 {
        assert_eq!(first, Booklet::new(&Convention::sayc(), 400, 2).to_string());
    }
}

#[test]
fn the_anomalies_point_at_pages() {
    let booklet = Booklet::new(&Convention::sayc(), 3000, 30);
    let anomalies = booklet.anomalies();
    assert!(!anomalies.is_empty());
    for anomaly in anomalies.iter() {
        let number: usize = anomaly
            .strip_prefix("Page ")
            .and_then(|rest| rest.split(':').next())
            .and_then(|n| n.parse().ok())
            .unwrap_or_else(|| panic!("{}", anomaly));
        assert!((1..=booklet.pages.len()).contains(&number), "{}", anomaly);
    }
    // Some hands fit a game that the robot never bids, because an earlier
    // entry of the table always wins.
    assert!(
        anomalies.iter().any(|a| a.contains("hands fit")),
        "{:#?}",
        anomalies
    );
}

#[test]
fn every_call_shows_the_hover_text_of_the_game() {
    let convention = Convention::sayc();
    let booklet = small_booklet();
    for page in booklet.pages.iter() {
        for bids in page.auctions.iter() {
            for meaning in page.table.iter() {
                let mut after = bids.clone();
                after.push(meaning.call);
                let hover = convention.refine(&after).unwrap().description();
                assert_eq!(
                    hover.into_string(),
                    meaning.description,
                    "{} after {}",
                    call(meaning.call),
                    auction(bids)
                );
            }
        }
    }
}

#[test]
fn a_summary_reads_as_one_line() {
    let html = "<strong>Stayman</strong><br/>8+ hcp<br/><br/><span>no</span> \
                &amp; so on<br/><br/><em>or</em><br/><strong>Other</strong><br/>";
    let html = html.replace("<br/><br/><em>or</em><br/>", super::OR);
    assert_eq!(one_line(&html), "Stayman; 8+ hcp; no & so on or Other");
}

#[test]
fn the_bars_add_up() {
    let booklet = small_booklet();
    for page in booklet.pages.iter() {
        // At every count of points, the calls made are the visits.
        for place in 0..POINTS {
            let made: u32 = page.made.values().map(|m| m[place]).sum();
            assert_eq!(made, page.visits[place], "{:?}", page.auctions[0]);
        }
        // And each call was made as often as the first pass counted.
        for line in booklet.lines(page) {
            let counted: u32 = page.made.get(&line.call).map_or(0, |m| m.iter().sum());
            assert_eq!(counted as u64, line.made, "{}", call(line.call));
        }
        let visits: u32 = page.visits.iter().sum();
        assert_eq!(visits as u64, booklet.reached(page));
    }
}

#[test]
fn the_html_booklet_has_every_page_and_links_to_them() {
    let booklet = small_booklet();
    let html = booklet.html("SAYC", &Booklet::fingerprint(&Convention::sayc()));
    for n in 1..=booklet.pages.len() {
        assert_eq!(html.matches(&format!("id=\"p{}\"", n)).count(), 1);
    }
    // Every link goes to a page that is there.
    for link in html.split("href=\"#p").skip(1) {
        let n: usize = link.split('"').next().unwrap().parse().unwrap();
        assert!((1..=booklet.pages.len()).contains(&n), "p{}", n);
    }
    // Every element that is opened is closed.
    for tag in [
        "section", "table", "tr", "th", "td", "span", "p", "i", "a", "h1", "h2", "strong", "style",
    ] {
        let opened = html.matches(&format!("<{} ", tag)).count()
            + html.matches(&format!("<{}>", tag)).count();
        assert_eq!(
            opened,
            html.matches(&format!("</{}>", tag)).count(),
            "{}",
            tag
        );
    }
    // The first page shows what the game shows for an opening of 1NT.
    let first = booklet.page_of[&Vec::new()];
    let hover = Convention::sayc()
        .refine(&[Bid::NT(1)])
        .unwrap()
        .description()
        .into_string();
    assert_eq!(first, 0);
    assert!(html.contains(&hover));
}

#[test]
fn the_fingerprint_does_not_change_between_runs() {
    let convention = Convention::sayc();
    assert_eq!(
        Booklet::fingerprint(&convention),
        Booklet::fingerprint(&convention)
    );
    assert!(Booklet::fingerprint(&convention) != Booklet::fingerprint(&Convention::sheets()));
}

/// Where the booklets of the bidding systems are kept: one HTML page each,
/// which the game's server embeds and serves.
#[cfg(test)]
fn booklet_path(system: super::BiddingSystem) -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("booklets")
        .join(format!("{}.html", system.slug()))
}

/// Writes the booklet of every bidding system, from 100,000 deals each:
/// `cargo test -p robot --release write_booklets -- --ignored`.  Run it
/// after changing a system's tables, and commit the result.
#[test]
#[ignore]
fn write_booklets() {
    std::fs::create_dir_all(booklet_path(super::BiddingSystem::Sayc).parent().unwrap()).unwrap();
    for system in super::BiddingSystem::ALL {
        let convention = system.convention();
        let booklet = Booklet::new(&convention, 100_000, 100);
        let html = booklet.html(system.name(), &Booklet::fingerprint(&convention));
        std::fs::write(booklet_path(system), html).unwrap();
    }
}

#[test]
fn the_booklets_match_the_tables() {
    // Each page records the fingerprint of the tables it was made from.  When
    // this fails, run `write_booklets` (see above) and commit the pages.
    for system in super::BiddingSystem::ALL {
        let html = std::fs::read_to_string(booklet_path(system))
            .unwrap_or_else(|e| panic!("no booklet for {}: {}", system.name(), e));
        let recorded = html
            .split("<!-- fingerprint: ")
            .nth(1)
            .and_then(|rest| rest.split(" -->").next())
            .unwrap_or("");
        assert_eq!(
            recorded,
            Booklet::fingerprint(&system.convention()),
            "the {} booklet is out of date",
            system.name()
        );
    }
}
