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
use super::{final_bid, BiddingSystem, Convention, Explanation, Forcing};
use crate::Bid;
use bridge_deck::HandValuation;
use display_as::{format_as, with_template, DisplayAs, HTML};
use std::collections::BTreeMap;
use wording::{and, at_least, Text, Word};

mod notation;
mod page;
mod walk;

use page::Page;
use walk::{rank, walk, Meanings, Node, Openings, Stage};

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

/// A booklet on a bidding system.
pub struct Booklet {
    system: BiddingSystem,
    parts: Vec<Part>,
    /// Whether the pages of responses show invitations that the contract
    /// rules make.
    invitations: bool,
    /// What the table says about how it bids.
    explanations: Vec<Explanation>,
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
            invitations: meanings.invites,
            explanations: table.explanations().to_vec(),
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
    use std::collections::HashSet;
    use std::sync::OnceLock;

    /// The booklet on `system`, written once for all the tests, since
    /// writing one takes seconds in a debug build.
    fn html(system: BiddingSystem) -> &'static str {
        static WRITTEN: [OnceLock<String>; BiddingSystem::ALL.len()] =
            [const { OnceLock::new() }; BiddingSystem::ALL.len()];
        let which = BiddingSystem::ALL
            .iter()
            .position(|&s| s == system)
            .unwrap();
        WRITTEN[which].get_or_init(|| format_as!(HTML, Booklet::new(system)).into_string())
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
            let ids = quoted(html, " id=\"");
            let unique: HashSet<&str> = ids.iter().copied().collect();
            assert_eq!(
                ids.len(),
                unique.len(),
                "{}: an id is used twice",
                system.name()
            );
            let links = quoted(html, " href=\"#");
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
        let again = format_as!(HTML, Booklet::new(BiddingSystem::Sheets)).into_string();
        assert!(html(BiddingSystem::Sheets) == again);
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
        let responses = page(html, "opened-2c-responses");
        let weak = row(responses, "Weak response");
        assert!(weak.starts_with("2♣ – 2♦\n"), "{weak}");
        let strong = row(responses, "Strong response");
        assert!(strong.contains("game forcing"), "{strong}");
        page(html, "opened-2c-opener-rebid");
    }

    #[test]
    fn opening_one_notrump() {
        let html = html(BiddingSystem::Sayc);
        let opening = row(page(html, "opening-bids"), "Opening 1NT");
        assert!(opening.contains("15–17 high card points"), "{opening}");
        let responses = page(html, "opened-1n-responses");
        let stayman = row(responses, "Stayman");
        assert!(stayman.starts_with("1NT – 2♣\n"), "{stayman}");
        row(responses, "Jacoby transfer");
    }

    /// Auctions that differ only in their suits are written once, with letters.
    #[test]
    fn suits_are_written_as_letters() {
        let html = html(BiddingSystem::Sayc);
        let responses = words(page(html, "opened-1c-responses"));
        assert!(responses.contains("\n1X – Pass\n"), "{responses}");
        assert!(responses.contains("\n1M – 2M\n"), "{responses}");
    }

    /// The booklet prints what a table says about how it bids, so only the
    /// systems that bid by the contract rules explain them.
    #[test]
    fn a_table_explains_how_it_bids() {
        let rules = "<h3>When the opponents have only passed</h3>";
        let sheets = html(BiddingSystem::Sheets);
        assert!(!sheets.contains(rules));
        assert!(sheets.contains("<h3>Natural bids</h3>"));
        let html = html(BiddingSystem::Sayc);
        let explained = words(&html[html.find(rules).expect("no contract rules")..]);
        assert!(
            explained.contains("a game in notrump or a major needs 25 points"),
            "{explained}"
        );
        assert!(explained.contains("Natural bids\n"), "{explained}");
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
