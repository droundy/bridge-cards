//! Benchmarks of the bidding systems, registered with `scaling` when the
//! `benchmarks` feature is on.  `benchmarks/` in this repository runs them.
//!
//! Each system is timed in four ways, one group for each: building its table,
//! finding what auctions mean, choosing bids, and writing its booklet.  Auctions
//! and hands are the seeded deals that the tests use, bid by the table
//! itself, and so are made when the input is, which is not timed.
//!
//! `cargo bench -- bidding` runs these and no others.

use super::{booklet::Booklet, seeded_deals, BiddingSystem, ConventionalBid};
use crate::Bid;
use bridge_deck::Cards;

/// How many seeded deals the benchmarks of meanings and bids go over.
const DEALS: u64 = 30;

/// A table, and the hands and auction of each of the seeded deals that it
/// bid.
#[derive(Clone)]
struct Deals {
    bidder: ConventionalBid,
    deals: Vec<(Vec<Cards>, Vec<Bid>)>,
}

fn deals(system: BiddingSystem) -> Deals {
    let table = system.convention();
    let deals = seeded_deals(table.clone(), DEALS)
        .into_iter()
        .map(|(_, hands, bids)| (hands, bids))
        .collect();
    Deals {
        bidder: ConventionalBid(table),
        deals,
    }
}

// The systems themselves, by their place in `BiddingSystem::ALL`, which are
// plain integers.

#[scaling::input(group("bidding: build", "bidding: booklet"), name = "Sheets", reusable)]
fn sheets_system() -> usize {
    0
}

#[scaling::input(group("bidding: build", "bidding: booklet"), name = "SAYC", reusable)]
fn sayc_system() -> usize {
    1
}

#[scaling::input(group("bidding: build", "bidding: booklet"), name = "2/1", reusable)]
fn two_over_one_system() -> usize {
    2
}

#[scaling::bench(group = "bidding: build", baseline)]
fn build_table(system: &usize) -> usize {
    BiddingSystem::ALL[*system].convention().entries().len()
}

#[scaling::bench(group = "bidding: booklet")]
fn write_booklet(system: &usize) -> Booklet {
    Booklet::new(BiddingSystem::ALL[*system])
}

#[scaling::input(
    group("bidding: meanings", "bidding: choose"),
    name = "Sheets",
    reusable
)]
fn sheets_deals() -> Deals {
    deals(BiddingSystem::Sheets)
}

#[scaling::input(group("bidding: meanings", "bidding: choose"), name = "SAYC", reusable)]
fn sayc_deals() -> Deals {
    deals(BiddingSystem::Sayc)
}

#[scaling::input(group("bidding: meanings", "bidding: choose"), name = "2/1", reusable)]
fn two_over_one_deals() -> Deals {
    deals(BiddingSystem::TwoOverOne)
}

/// What every call of every auction means, with nothing remembered at first.
#[scaling::bench(group = "bidding: meanings", baseline)]
fn refine_calls(input: &Deals) -> usize {
    let table = &input.bidder.0;
    table.forget_meanings();
    let mut meant = 0;
    for (_, bids) in &input.deals {
        for n in 1..=bids.len() {
            meant += usize::from(table.refine(&bids[..n]).is_some());
        }
    }
    meant
}

/// The bid for every hand that bid in every auction, with nothing remembered
/// at first.
#[scaling::bench(group = "bidding: choose", baseline)]
fn choose_bids(input: &Deals) -> usize {
    input.bidder.0.forget_meanings();
    let mut bid = 0;
    for (hands, bids) in &input.deals {
        for n in 0..bids.len() {
            bid += usize::from(input.bidder.choose(&bids[..n], hands[n % 4]).is_some());
        }
    }
    bid
}
