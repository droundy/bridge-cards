//! A sketch of a booklet that explains a bidding system the way a
//! choose-your-own-adventure book does.  Each page starts from an auction and
//! lists the calls that may come next, most likely first, each with the page
//! that follows it.
//!
//! A page holds every auction after which the system gives each call the same
//! meaning, so one page covers an opening in any seat, or a convention that is
//! reached in several ways.  Which auctions come up, and how often each call is
//! made, come from letting the robot bid seeded random deals.

use super::{robot_choice, seeded_hands, Convention, Described, Forcing, ShapeRule};
use crate::Bid;
use bridge_deck::HandValuation;
#[cfg(test)]
use display_as::{format_as, HTML};
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
}

/// One way a hand can fit a call: bounds on its values, and rules about its
/// shape.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct Way {
    min: HandValuation,
    max: HandValuation,
    shape: Vec<ShapeRule>,
}

impl Way {
    /// The ways a hand can fit `convention`.
    pub(super) fn of(convention: &Convention) -> Vec<Way> {
        match convention {
            Convention::Simple { min, max, .. }
            | Convention::Natural { min, max, .. }
            | Convention::Forced { min, max, .. } => vec![Way {
                min: *min,
                max: *max,
                shape: Vec::new(),
            }],
            Convention::Rule { fits, .. } => fits
                .iter()
                .map(|f| Way {
                    min: f.min,
                    max: f.max,
                    shape: f.shape.clone(),
                })
                .collect(),
            Convention::Either(options) => options.iter().flat_map(Way::of).collect(),
            Convention::Ordered { .. } => Vec::new(),
        }
    }
}

/// What a call means after some auction: the convention it matches, how
/// forcing it is, and every way a hand can fit it once the auction has refined
/// the bounds, in the order of the table entries that say so, since that is
/// the order the robot goes by.  Auctions whose calls all mean the same go on
/// one page.
#[derive(Clone, PartialEq, Eq, Hash)]
struct Meaning {
    call: Bid,
    name: String,
    forcing: Forcing,
    ways: Vec<Way>,
    /// The bounds that every way lies within, which the sketch shows.
    min: HandValuation,
    max: HandValuation,
}

impl Meaning {
    fn new(described: &Described) -> Self {
        let (min, max) = described.meaning.bounds();
        Meaning {
            call: described.bid,
            name: described.meaning.name().into_string(),
            forcing: described.meaning.is_forcing(),
            ways: described.ways(),
            min,
            max,
        }
    }
}

struct Page {
    /// The auctions this page covers, most often reached first.
    auctions: Vec<Vec<Bid>>,
    /// The calls the system understands after them, in the robot's order.
    table: Vec<Meaning>,
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
        for seed in 0..deals {
            let hands = seeded_hands(seed);
            let mut bids = Vec::new();
            while !auction_over(&bids) {
                let auction = auctions.entry(bids.clone()).or_insert_with(|| Auction {
                    described: convention.described_bids(&bids),
                    reached: 0,
                    calls: HashMap::new(),
                });
                let hand = hands[bids.len() % 4].values();
                let call = robot_choice(&auction.described, &hand).unwrap_or(Bid::Pass);
                auction.reached += 1;
                *auction.calls.entry(call).or_default() += 1;
                bids.push(call);
            }
        }
        auctions.retain(|_, a| a.reached >= min_reached);

        let mut by_table: HashMap<Vec<Meaning>, Vec<Vec<Bid>>> = HashMap::new();
        for (bids, auction) in auctions.iter() {
            // The calls in the order the robot considers them: by where the
            // table first mentions them, and the higher call first when it
            // is the same entry.
            let mut in_order: Vec<&Described> = auction.described.iter().collect();
            in_order.sort_by_key(|d| d.position());
            let table = in_order.into_iter().map(Meaning::new);
            by_table
                .entry(table.collect())
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
        for (table, mut members) in by_table {
            members.sort_by_key(|bids| std::cmp::Reverse(booklet.auctions[bids].reached));
            booklet.pages.push(Page {
                auctions: members,
                table,
            });
        }
        booklet.put_in_reading_order();
        booklet
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
        f.write_str(&self.name)?;
        if (self.min.hcp, self.max.hcp) != (HandValuation::MIN.hcp, HandValuation::MAX.hcp) {
            write!(f, ", {}–{} hcp", self.min.hcp, self.max.hcp)?;
        }
        match self.forcing {
            Forcing::GameForcing => f.write_str(", game forcing"),
            Forcing::Forcing => f.write_str(", forcing"),
            Forcing::Forced => f.write_str(", forced"),
            Forcing::Passable => Ok(()),
        }
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

#[test]
fn calls_with_different_shape_rules_do_not_mean_the_same() {
    use super::Fit;
    use bridge_deck::Suit::{Hearts, Spades};
    use regex::RegexSet;
    let entry = |rule: ShapeRule| Convention::Rule {
        regex: RegexSet::new(["1S"]).unwrap(),
        the_description: format_as!(HTML, ""),
        the_name: "Opening bid",
        fits: vec![Fit::new(HandValuation::MIN, HandValuation::MAX).with(rule)],
        forcing: Forcing::Passable,
    };
    let described = |rule: ShapeRule| {
        let meaning = entry(rule);
        Described::new(Bid::Pass, meaning.clone(), vec![(0, meaning)])
    };
    let longer = Meaning::new(&described(ShapeRule::Longer(Spades, Hearts)));
    let as_long = Meaning::new(&described(ShapeRule::AtLeastAsLong(Spades, Hearts)));
    // The name and the hull of the bounds are the same.
    assert_eq!(longer.name, as_long.name);
    assert_eq!((longer.min, longer.max), (as_long.min, as_long.max));
    assert!(longer != as_long);
    assert!(longer == Meaning::new(&described(ShapeRule::Longer(Spades, Hearts))));
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
    println!(
        "{}",
        Booklet::new(&system.convention(), deals, deals / 1000)
    );
}
