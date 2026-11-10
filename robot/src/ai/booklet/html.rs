//! A booklet as one HTML page: every page of the booklet one after another,
//! each with its calls, how likely each is, a bar showing at which count of
//! points it is made, the game's hover summary of it, and links to the pages
//! that follow.  There are no scripts, so it can be served as a file, shown in
//! a frame in the game, or printed.

use super::{auction, call, Booklet, Page, POINTS};
use crate::Bid;
use display_as::{format_as, HTML};

/// What the page looks like, in light and dark.
const STYLE: &str = r#"
:root { --text: #222; --background: #fff; --faint: #aaa; --bar: #2a7; --link: #05c;
        --clubs: #181; --diamonds: #c60; --hearts: #c00; --spades: #222;
        --k-pass: #e4e2dc; --k-nt: #a9cdf0; --k-major: #cbc5f5; --k-minor: #9fdcc4;
        --k-suit: #c9e4a5; --k-x: #f6cf82; --k-xx: #f5b9a2; }
@media (prefers-color-scheme: dark) {
  :root { --text: #eee; --background: #1e1a18; --faint: #777; --bar: #5c9; --link: #8bf;
          --clubs: #7d7; --diamonds: #fa5; --hearts: #f66; --spades: #eee;
          --k-pass: #3a3633; --k-nt: #2c4a6b; --k-major: #403a6e; --k-minor: #1f5a47;
          --k-suit: #3f5a24; --k-x: #6b5320; --k-xx: #6e3a2a; }
}
body { font-family: sans-serif; color: var(--text); background: var(--background);
       margin: 0 auto; max-width: 60em; padding: 0 0.5em; }
.clubs { color: var(--clubs); } .diamonds { color: var(--diamonds); }
.hearts { color: var(--hearts); } .spades { color: var(--spades); }
section { margin: 1.5em 0; padding-top: 0.5em; border-top: 1px solid var(--faint); }
section:target { background: color-mix(in srgb, var(--bar) 12%, var(--background)); }
h2 { margin: 0; font-size: 1.1em; }
.after { margin: 0.2em 0; color: var(--faint); font-size: 0.85em; }
table { border-collapse: collapse; }
th, td { padding: 0.1em 0.5em 0.1em 0; text-align: left; vertical-align: middle; }
a { color: var(--link); }
td.chance { text-align: right; font-variant-numeric: tabular-nums; }
.bar { display: grid; grid-template-columns: repeat(41, 0.6em); }
.bar i { height: 1.2em; background: var(--bar); opacity: calc(var(--s) / 100); }
.bar i.n { background: none; opacity: 1; border-bottom: 1px dotted var(--faint); }
.axis { display: grid; grid-template-columns: repeat(41, 0.6em); font-size: 0.7em;
        color: var(--faint); }
.axis span { grid-row: 1; }
.blocks { display: block; max-width: 100%; height: auto; margin: 0.4em 0; }
.blocks text { font: 12px sans-serif; fill: var(--text); text-anchor: middle;
               dominant-baseline: central; pointer-events: none; }
.blocks .range { font-size: 10px; }
.blocks .small { font-size: 9px; }
.blocks .tick { font-size: 10px; fill: var(--faint); }
.blocks .edge { stroke: var(--background); stroke-width: 2.5; fill: none; }
.blocks a { cursor: pointer; }
.blocks .clubs { fill: var(--clubs); } .blocks .diamonds { fill: var(--diamonds); }
.blocks .hearts { fill: var(--hearts); } .blocks .spades { fill: var(--spades); }
.k-pass path { fill: var(--k-pass); } .k-nt path { fill: var(--k-nt); }
.k-major path { fill: var(--k-major); } .k-minor path { fill: var(--k-minor); }
.k-suit path { fill: var(--k-suit); } .k-x path { fill: var(--k-x); } .k-xx path { fill: var(--k-xx); }
.hastooltip { position: relative; display: inline-block; cursor: help; }
.tooltip { visibility: hidden; position: absolute; z-index: 1; left: 2.5em; top: 1.2em;
           width: 18em; padding: 0.3em 0.5em; border-radius: 6px; font-weight: normal;
           background: var(--background); color: var(--text); font-size: 0.85em;
           box-shadow: 0 0 0.3em var(--text); }
.hastooltip:hover .tooltip { visibility: visible; }
.go { font-size: 0.85em; }
.never { font-size: 0.8em; color: var(--faint); }
"#;

/// `text` as it goes in HTML.
pub(super) fn escape(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// A call as HTML, with the suit in colour.
fn call_html(bid: Bid) -> String {
    format_as!(HTML, bid).into_string()
}

/// The links to `pages`, as in "p3/p17", at most three.
fn links(pages: &[usize]) -> String {
    let mut shown: Vec<String> = pages
        .iter()
        .take(3)
        .map(|p| format!("<a href=\"#p{0}\">p{0}</a>", p + 1))
        .collect();
    if pages.len() > 3 {
        shown.push("…".to_string());
    }
    shown.join("/")
}

impl Booklet {
    /// The booklet for `title`, such as the name of the bidding system, as an
    /// HTML page.  `fingerprint` goes in a comment, so that a page that no
    /// longer matches the tables can be recognized.
    pub fn html(&self, title: &str, fingerprint: &str) -> String {
        let title = escape(title);
        let mut out = String::new();
        out.push_str("<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">\n");
        out.push_str("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
        out.push_str(&format!(
            "<title>{} booklet</title>\n<style>{}</style>\n",
            title, STYLE
        ));
        out.push_str(&format!(
            "<!-- fingerprint: {} -->\n</head><body>\n",
            fingerprint
        ));
        out.push_str(&format!(
            "<h1>{} booklet</h1>\n<p>{} pages for the {} auctions that came up at least {} times \
             in {} deals the robot bid against itself.  Each page is the choice after the \
             auctions it lists.  A page opens with a table of blocks: the points run along \
             it, each row is a kind of hand, and each block is the call the robot makes with \
             such a hand and that many points.  Click a block for partner's reply, or point \
             the mouse at it for what the system says it means.  Under the table the calls \
             are in order of how often they were made, and a bar shows at which count of \
             points: the darker the block, the larger the share of hands with that many \
             points that make the call.</p>\n",
            title,
            self.pages.len(),
            self.auctions.len(),
            self.min_reached,
            self.deals,
        ));
        for (n, page) in self.pages.iter().enumerate() {
            self.page_html(&mut out, n, page);
        }
        out.push_str("</body></html>\n");
        out
    }

    /// One page of the booklet, as a section.
    fn page_html(&self, out: &mut String, n: usize, page: &Page) {
        const SHOWN: usize = 6;
        let reached = self.reached(page);
        out.push_str(&format!(
            "<section id=\"p{0}\"><h2>Page {0}</h2>\n<p class=\"after\">Reached {1} times, after: ",
            n + 1,
            reached
        ));
        let mut shown: Vec<String> = page
            .auctions
            .iter()
            .take(SHOWN)
            .map(|bids| escape(&auction(bids)))
            .collect();
        if page.auctions.len() > SHOWN {
            shown.push(format!("and {} more", page.auctions.len() - SHOWN));
        }
        out.push_str(&shown.join(" · "));
        out.push_str("</p>\n");
        out.push_str(&self.block_table(page));
        out.push_str("<table>\n");
        out.push_str(&format!(
            "<tr><th></th><th></th><td class=\"axis\">{}</td><th></th></tr>\n",
            (0..POINTS)
                .step_by(10)
                .map(|p| format!("<span style=\"grid-column: {}\">{}</span>", p + 1, p))
                .collect::<String>()
        ));
        let mut never = Vec::new();
        for line in self.lines(page) {
            let Some(made) = page.made.get(&line.call).filter(|_| line.made > 0) else {
                never.push(call(line.call));
                continue;
            };
            let description = line
                .meaning
                .map_or("Nothing else fits, so the robot passes.".to_string(), |m| {
                    m.description.clone()
                });
            let cells: String = (0..POINTS)
                .map(|p| {
                    if page.visits[p] == 0 {
                        "<i class=\"n\"></i>".to_string()
                    } else {
                        let share = 100 * made[p] as u64 / page.visits[p] as u64;
                        format!("<i style=\"--s:{}\"></i>", share)
                    }
                })
                .collect();
            let mut go = String::new();
            if !line.reply.is_empty() {
                go.push_str(&format!("→ {}", links(&line.reply)));
            }
            if !line.next.is_empty() {
                go.push_str(&format!(" (they may bid: {})", links(&line.next)));
            }
            out.push_str(&format!(
                "<tr><th><span class=\"hastooltip\">{}<span class=\"tooltip\">{}</span></span></th>\
                 <td class=\"chance\">{:.1}%</td><td class=\"bar\">{}</td><td class=\"go\">{}</td></tr>\n",
                call_html(line.call),
                description,
                100.0 * line.made as f64 / reached as f64,
                cells,
                go
            ));
        }
        out.push_str("</table>\n");
        out.push_str(&format!(
            "<p class=\"never\">Bars show {}.{}</p>\n</section>\n",
            page.points.name(),
            if never.is_empty() {
                String::new()
            } else {
                format!(" Understood but never made: {}.", never.join(" "))
            }
        ));
    }
}
