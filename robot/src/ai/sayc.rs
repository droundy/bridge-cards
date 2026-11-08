//! Standard American Yellow Card (SAYC), and its two-over-one game-force
//! cousin, as bidding conventions.
//!
//! SAYC follows the ACBL's "SAYC System Booklet" (revised January 2006).
//! Both are built by the same function.  They agree on openings, notrump
//! structure, the strong two clubs, weak twos and competitive bidding, and
//! differ in how responder shows a hand:
//!
//! | | SAYC | Two over one |
//! |-|------|--------------|
//! | 1NT over a major | 6-9 hcp, passable | 6-12 hcp, forcing |
//! | limit raise of a major | 10-11 hcp | 10-12 hcp |
//! | new suit at the two level | 10+ hcp, forcing one round | 12+ hcp, game forcing |
//! | opener's rebid after 1NT | pass with a minimum | must bid |
//!
//! The table is read in order (see `Convention`): for each auction the
//! entries come in the order a player would consider them, and the robot
//! bids the first that fits its hand.  A later entry for the same bid is a
//! fallback, such as opener rebidding a five card suit when nothing else
//! fits.
//!
//! Where the table still differs from the booklet:
//!
//! * Raises count high card points, not the booklet's dummy points.
//! * Notrump bids do not check for a stopper.
//! * 2H over 1S promises five hearts.
//! * In two over one the direct limit raise 3M takes three trumps, though
//!   the usual way to show three is 1NT and then a jump.
//!
//! Not covered, though the booklet has them: slam conventions (Blackwood and
//! Gerber), negative doubles, strong jump shifts, jump shifts by opener, a
//! 3NT opening and Michaels over a major.  (Splinters are not part of SAYC.)
//! Auctions the tables do not mention fall back on the natural bids shared
//! with the other conventions.
use super::{Convention, Fit, Forcing, ShapeRule};
use bridge_deck::{HandValuation, Suit};
use display_as::{format_as, HTML};
use regex::RegexSet;
use ShapeRule::{AtLeastAsLong, Balanced, Longer};

/// Passes before the first bid of the auction.
const OPEN: &str = "(P )*";

impl Convention {
    /// Standard American Yellow Card.
    pub fn sayc() -> Self {
        standard_american("SAYC", false)
    }
    /// Two-over-one game force, built on the same openings as SAYC.
    pub fn two_over_one() -> Self {
        standard_american("2/1", true)
    }
}

trait Shape: Sized {
    /// Require at least `n` cards in `suit`, on a lower bound.  It never
    /// loosens a bound that already requires more.
    fn min_len(self, suit: Suit, n: u8) -> Self;
    /// Allow at most `n` cards in `suit`, on an upper bound.  It never
    /// loosens a bound that already allows fewer.
    fn max_len(self, suit: Suit, n: u8) -> Self;
}

impl Shape for HandValuation {
    fn min_len(mut self, suit: Suit, n: u8) -> Self {
        self.length[suit] = self.length[suit].max(n);
        self
    }
    fn max_len(mut self, suit: Suit, n: u8) -> Self {
        self.length[suit] = self.length[suit].min(n);
        self
    }
}

/// A convention under construction.
struct Sheet(Convention);

impl Sheet {
    /// An entry that a hand fits if it fits any of `fits`.
    fn add(
        &mut self,
        name: &'static str,
        regex: &str,
        description: &str,
        forcing: Forcing,
        fits: impl IntoIterator<Item = Fit>,
    ) {
        self.0.add(Convention::Rule {
            regex: RegexSet::new([regex]).unwrap(),
            the_description: format_as!(HTML, description),
            the_name: name,
            fits: fits.into_iter().collect(),
            forcing,
        });
    }
    /// The entries that `add` makes, which were written for the rule
    /// Sheets bids by (see `Convention::most_forcing_then_highest`).
    fn add_block(&mut self, add: impl FnOnce(&mut Convention)) {
        let mut block = Convention::new("");
        add(&mut block);
        self.0.append(block.most_forcing_then_highest());
    }
}

const MIN: HandValuation = HandValuation::MIN;
const MAX: HandValuation = HandValuation::MAX;

/// A hand within `min` and `max`.
fn fit(min: HandValuation, max: HandValuation) -> Fit {
    Fit::new(min, max)
}

/// A balanced hand within `min` and `max`.
fn balanced(mut min: HandValuation, mut max: HandValuation) -> Fit {
    for suit in Suit::ALL.iter().cloned() {
        min = min.min_len(suit, 2);
        max = max.max_len(suit, 5);
    }
    fit(min, max).with(Balanced)
}

/// A balanced hand of `lo` to `hi` hcp.
fn balanced_hcp(lo: u8, hi: u8) -> Fit {
    balanced(MIN.with_hcp(lo), MAX.with_hcp(hi))
}

/// The level at which `suit` can be bid over a bid in `over`.
fn cheapest_level(suit: Suit, over: Suit) -> usize {
    if suit > over {
        1
    } else {
        2
    }
}

fn is_major(suit: Suit) -> bool {
    suit == Suit::Hearts || suit == Suit::Spades
}

/// The suits other than `except`, highest first.
fn suits_except(except: &[Suit]) -> impl Iterator<Item = Suit> + '_ {
    Suit::ALL
        .iter()
        .rev()
        .cloned()
        .filter(move |s| !except.contains(s))
}

/// `fit`, with `suit` at least as long as each lower suit of `others`.  When
/// suits are offered highest first, the longest is bid, and the higher of
/// two of the same length.
fn longest(mut fit: Fit, suit: Suit, others: impl Iterator<Item = Suit>) -> Fit {
    for other in others.filter(|&other| other < suit) {
        fit = fit.with(AtLeastAsLong(suit, other));
    }
    fit
}

fn standard_american(name: &'static str, two_over_one: bool) -> Convention {
    let mut s = Sheet(Convention::new(name));
    s.add_block(|t| t.add_strong_two_clubs(22));
    s.add_block(|t| t.add_notrump((21, 22, 22), 8, Some(Balanced)));
    s.add_block(|t| t.add_preempts());
    openings(&mut s);
    responses(&mut s, two_over_one);
    opener_rebids(&mut s, two_over_one);
    competition(&mut s);
    s.add_block(|t| t.add_natural_bids());
    s.0
}

/// One of a suit, after the strong 2C, the notrump openings and the weak
/// twos and threes, which come first in the table.
fn openings(s: &mut Sheet) {
    use Suit::*;
    // Hands of 13 to 21 points open one of a suit.
    let (min, max) = (MIN.with_lhcp(13), MAX.with_lhcp(21));
    let no_major = max.max_len(Hearts, 4).max_len(Spades, 4);

    for major in [Spades, Hearts].iter().cloned() {
        s.add(
            "Preemptive opening",
            &format!("^{OPEN}4{}$", major.latin()),
            "8+ card suit",
            Forcing::Passable,
            [fit(MIN.with_hcp(5).min_len(major, 8), MAX.with_hcp(11))],
        );
    }
    // The longer major, and the higher of two of the same length.
    s.add(
        "Opening bid",
        &format!("^{OPEN}1S$"),
        "5+ spades",
        Forcing::Passable,
        [fit(min.min_len(Spades, 5), max).with(AtLeastAsLong(Spades, Hearts))],
    );
    s.add(
        "Opening bid",
        &format!("^{OPEN}1H$"),
        "5+ hearts",
        Forcing::Passable,
        [fit(min.min_len(Hearts, 5), max).with(Longer(Hearts, Spades))],
    );
    // 1D with longer diamonds, which with three of them can only be four
    // spades, four hearts, three diamonds and two clubs, or with 4-4 or
    // longer in both minors.
    s.add(
        "Opening bid",
        &format!("^{OPEN}1D$"),
        "No 5 card major, more diamonds than clubs, or 4-4",
        Forcing::Passable,
        [
            fit(min.min_len(Diamonds, 3), no_major).with(Longer(Diamonds, Clubs)),
            fit(min.min_len(Diamonds, 4), no_major).with(AtLeastAsLong(Diamonds, Clubs)),
        ],
    );
    s.add(
        "Opening bid",
        &format!("^{OPEN}1C$"),
        "No 5 card major, more clubs than diamonds, or 3-3",
        Forcing::Passable,
        [fit(min.min_len(Clubs, 3), no_major)],
    );
    s.add(
        "Opening pass",
        &format!("^{OPEN}P$"),
        "",
        Forcing::Passable,
        [fit(MIN, MAX.with_lhcp(12))],
    );
}

/// Responder's bids after a one-level opening, when the opponents pass or
/// double.
fn responses(s: &mut Sheet, two_over_one: bool) {
    use Suit::*;
    let weak = MIN.with_hcp(6);
    let no_major = MAX.max_len(Hearts, 3).max_len(Spades, 3);
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        let after = format!("^{OPEN}1{o} [PX] ");
        // New suits at the two level, the longest first.
        let two_level = |s: &mut Sheet| {
            let (strength, forcing) = if two_over_one {
                (12, Forcing::GameForcing)
            } else {
                (10, Forcing::Forcing)
            };
            for response in suits_except(&[opening]).filter(|&r| r < opening) {
                let length = if response == Hearts || opening == Diamonds {
                    5
                } else {
                    4
                };
                let min = MIN.with_hcp(strength).min_len(response, length);
                s.add(
                    "Two-level response",
                    &format!("{after}2{}$", response.latin()),
                    "New suit",
                    forcing,
                    [longest(fit(min, MAX), response, suits_except(&[opening]))],
                );
            }
        };

        if is_major(opening) {
            s.add(
                "Jacoby 2NT",
                &format!("{after}2N$"),
                "4+ card support",
                Forcing::GameForcing,
                [fit(MIN.with_hcp(13).min_len(opening, 4), MAX)],
            );
            s.add(
                "Response",
                &format!("{after}3N$"),
                "Balanced, two card support",
                Forcing::Passable,
                [balanced(
                    MIN.with_hcp(15).min_len(opening, 2),
                    MAX.with_hcp(17).max_len(opening, 2),
                )],
            );
            s.add(
                "Limit raise",
                &format!("{after}3{o}$"),
                "3+ card support",
                Forcing::Passable,
                [fit(
                    MIN.with_hcp(10).min_len(opening, 3),
                    MAX.with_hcp(if two_over_one { 12 } else { 11 }),
                )],
            );
            if opening == Hearts {
                s.add(
                    "Response",
                    &format!("{after}1S$"),
                    "4+ spades",
                    Forcing::Forcing,
                    [fit(weak.min_len(Spades, 4), MAX)],
                );
            }
            two_level(s);
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                "3+ card support",
                Forcing::Passable,
                [fit(weak.min_len(opening, 3), MAX.with_hcp(9))],
            );
            s.add(
                "Response",
                &format!("{after}1N$"),
                "No fit and no suit to bid",
                if two_over_one {
                    Forcing::Forcing
                } else {
                    Forcing::Passable
                },
                [fit(weak, MAX.with_hcp(if two_over_one { 12 } else { 9 }))],
            );
        } else {
            s.add(
                "Response",
                &format!("{after}2N$"),
                "Balanced, no 4 card major",
                Forcing::GameForcing,
                [balanced(MIN.with_hcp(13), no_major.with_hcp(15))],
            );
            s.add(
                "Response",
                &format!("{after}3N$"),
                "Balanced, no 4 card major",
                Forcing::Passable,
                [balanced(MIN.with_hcp(16), no_major.with_hcp(18))],
            );
            // A major before a raise of the minor: up the line with 4-4, the
            // longer with 5-4, and spades with 5-5.
            s.add(
                "Response",
                &format!("{after}1S$"),
                "4+ spades",
                Forcing::Forcing,
                [
                    fit(weak.min_len(Spades, 4), MAX).with(Longer(Spades, Hearts)),
                    fit(weak.min_len(Spades, 5), MAX).with(AtLeastAsLong(Spades, Hearts)),
                ],
            );
            s.add(
                "Response",
                &format!("{after}1H$"),
                "4+ hearts",
                Forcing::Forcing,
                [fit(weak.min_len(Hearts, 4), MAX)],
            );
            if opening == Clubs {
                s.add(
                    "Response",
                    &format!("{after}1D$"),
                    "4+ diamonds",
                    Forcing::Forcing,
                    [fit(weak.min_len(Diamonds, 4), MAX)],
                );
            }
            two_level(s);
            // Four trumps raise 1D, but five raise 1C.
            let (support, shown) = if opening == Diamonds {
                (4, "4+ card support")
            } else {
                (5, "5+ card support")
            };
            s.add(
                "Limit raise",
                &format!("{after}3{o}$"),
                shown,
                Forcing::Passable,
                [fit(
                    MIN.with_hcp(10).min_len(opening, support),
                    MAX.with_hcp(12),
                )],
            );
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                shown,
                Forcing::Passable,
                [fit(weak.min_len(opening, support), MAX.with_hcp(9))],
            );
            s.add(
                "Response",
                &format!("{after}1N$"),
                "No fit and no suit to bid",
                Forcing::Passable,
                [fit(weak, MAX.with_hcp(10))],
            );
        }
    }
    // Responder may not pass with six or more points, so when nothing else
    // fits there is a forcing bid that may be short, and then 1NT.
    for opening in Suit::ALL.iter().cloned() {
        let after = format!("^{OPEN}1{} [PX] ", opening.latin());
        if opening == Clubs {
            s.add(
                "Response",
                &format!("{after}1D$"),
                "Nothing else fits, may be short",
                Forcing::Forcing,
                [fit(weak, MAX)],
            );
        } else {
            s.add(
                "Two-level response",
                &format!("{after}2C$"),
                "Nothing else fits, may be short",
                if two_over_one {
                    Forcing::GameForcing
                } else {
                    Forcing::Forcing
                },
                [fit(MIN.with_hcp(if two_over_one { 12 } else { 10 }), MAX)],
            );
        }
        s.add(
            "Response",
            &format!("{after}1N$"),
            "Nothing else fits",
            Forcing::Passable,
            [fit(weak, MAX)],
        );
    }
    s.add(
        "Response pass",
        &format!("^{OPEN}1. P P$"),
        "",
        Forcing::Passable,
        [fit(MIN, MAX.with_hcp(5))],
    );
}

/// Opener's second bid.
///
/// The booklet's ranges are a minimum of 13-15, a medium hand of 16-18
/// (jump raises and jump rebids, and reverses) and a non-reverse new suit of
/// 13-18.  After a one level response only a reverse forces, but after a two
/// level response responder has promised to bid again, so every rebid does.
fn opener_rebids(s: &mut Sheet, two_over_one: bool) {
    let min = MIN.with_lhcp(13);
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        for response in suits_except(&[opening]) {
            let level = cheapest_level(response, opening);
            let r = response.latin();
            let after = format!("^{OPEN}1{o} P {level}{r} P ");
            let rebid_forcing = if level == 2 {
                Forcing::Forcing
            } else {
                Forcing::Passable
            };
            // New suits, highest first, with their level and whether each is
            // a reverse.
            let new_suits: Vec<(usize, Suit, bool)> = suits_except(&[opening, response])
                .map(|z| {
                    let zlevel = if z > response { level } else { level + 1 };
                    (zlevel, z, zlevel == 2 && z > opening)
                })
                .collect();
            let new_suit = |s: &mut Sheet, zlevel: usize, z: Suit| {
                s.add(
                    "New suit",
                    &format!("{after}{zlevel}{}$", z.latin()),
                    "4+ cards",
                    rebid_forcing,
                    [fit(
                        min.min_len(z, 4),
                        MAX.with_lhcp(if zlevel == 2 { 18 } else { 21 }),
                    )],
                );
            };

            for &(zlevel, z, reverse) in new_suits.iter() {
                if reverse {
                    s.add(
                        "New suit",
                        &format!("{after}{zlevel}{}$", z.latin()),
                        "Reverse",
                        Forcing::Forcing,
                        [fit(
                            MIN.with_lhcp(16).min_len(opening, 5).min_len(z, 4),
                            MAX.with_lhcp(21),
                        )],
                    );
                }
            }
            s.add(
                "Jump rebid",
                &format!("{after}3{o}$"),
                "6+ card suit, invitational",
                rebid_forcing,
                [fit(
                    MIN.with_lhcp(16).min_len(opening, 6),
                    MAX.with_lhcp(18),
                )],
            );
            if level == 1 {
                s.add(
                    "Raise",
                    &format!("{after}3{r}$"),
                    "4+ card support, invitational",
                    Forcing::Passable,
                    [fit(
                        MIN.with_lhcp(16).min_len(response, 4),
                        MAX.with_lhcp(18),
                    )],
                );
                s.add(
                    "Jump rebid",
                    &format!("{after}2N$"),
                    "Balanced, 18-19 hcp",
                    Forcing::Passable,
                    [balanced_hcp(18, 19)],
                );
                // Both partners promised four cards, so opener needs four to
                // raise.
                s.add(
                    "Raise",
                    &format!("{after}2{r}$"),
                    "4+ card support",
                    Forcing::Passable,
                    [fit(min.min_len(response, 4), MAX.with_lhcp(15))],
                );
                // A suit at the one level comes before notrump, and up the
                // line.
                for &(zlevel, z, _) in new_suits.iter().rev() {
                    if zlevel == 1 {
                        new_suit(s, zlevel, z);
                    }
                }
                s.add(
                    "Rebid",
                    &format!("{after}1N$"),
                    "Balanced minimum",
                    Forcing::Passable,
                    [balanced_hcp(13, 15)],
                );
                s.add(
                    "Rebid",
                    &format!("{after}2{o}$"),
                    "6+ card suit",
                    Forcing::Passable,
                    [fit(min.min_len(opening, 6), MAX.with_lhcp(15))],
                );
                for &(zlevel, z, reverse) in new_suits.iter() {
                    if zlevel == 2 && !reverse {
                        new_suit(s, zlevel, z);
                    }
                }
            } else {
                s.add(
                    "Raise",
                    &format!("{after}3{r}$"),
                    "Support",
                    rebid_forcing,
                    [fit(
                        min.min_len(response, if is_major(response) { 3 } else { 4 }),
                        MAX,
                    )],
                );
                s.add(
                    "Rebid",
                    &format!("{after}2{o}$"),
                    "6+ card suit",
                    rebid_forcing,
                    [fit(min.min_len(opening, 6), MAX.with_lhcp(15))],
                );
                for &(zlevel, z, reverse) in new_suits.iter() {
                    if !reverse {
                        new_suit(s, zlevel, z);
                    }
                }
                s.add(
                    "Rebid",
                    &format!("{after}2N$"),
                    "Balanced minimum",
                    rebid_forcing,
                    [balanced_hcp(13, 15)],
                );
                if !is_major(response) {
                    // Three card support, when the hand is not balanced.
                    s.add(
                        "Raise",
                        &format!("{after}3{r}$"),
                        "3 card support",
                        rebid_forcing,
                        [fit(min.min_len(response, 3), MAX)],
                    );
                }
            }
            // With nothing else to bid, opener rebids a five card suit.
            s.add(
                "Rebid",
                &format!("{after}2{o}$"),
                "5+ cards, nothing else fits",
                rebid_forcing,
                [fit(min.min_len(opening, 5), MAX)],
            );
        }

        // After a 1NT response to a major, where opener's bids are not
        // forcing, though two over one's 1NT forces opener to make one.
        if is_major(opening) {
            let after = format!("^{OPEN}1{o} P 1N P ");
            s.add(
                "Jump rebid",
                &format!("{after}3{o}$"),
                "6+ card suit, invitational",
                Forcing::Passable,
                [fit(
                    MIN.with_lhcp(16).min_len(opening, 6),
                    MAX.with_lhcp(18),
                )],
            );
            s.add(
                "Rebid",
                &format!("{after}2N$"),
                "Balanced, 18-19 hcp",
                Forcing::Passable,
                [balanced_hcp(18, 19)],
            );
            for z in suits_except(&[opening]).filter(|&z| z > opening) {
                s.add(
                    "New suit",
                    &format!("{after}2{}$", z.latin()),
                    "Reverse",
                    Forcing::Forcing,
                    [fit(
                        MIN.with_lhcp(16).min_len(opening, 5).min_len(z, 4),
                        MAX,
                    )],
                );
            }
            s.add(
                "Rebid",
                &format!("{after}2{o}$"),
                "6+ card suit",
                Forcing::Passable,
                [fit(min.min_len(opening, 6), MAX.with_lhcp(15))],
            );
            for z in suits_except(&[opening]).filter(|&z| z < opening) {
                s.add(
                    "New suit",
                    &format!("{after}2{}$", z.latin()),
                    "New suit",
                    Forcing::Passable,
                    [fit(min.min_len(z, 4), MAX)],
                );
            }
            if two_over_one {
                // Opener may not pass the forcing notrump, so 2C is what is
                // left when nothing else fits, and may be short.
                s.add(
                    "New suit",
                    &format!("{after}2C$"),
                    "Nothing else fits, may be short",
                    Forcing::Passable,
                    [fit(MIN, MAX)],
                );
            }
        }
    }
}

/// Overcalls, takeout doubles and the two-suited overcalls.
fn competition(s: &mut Sheet) {
    use Suit::*;
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        // Michaels over a minor shows both majors.  Over a major it shows the
        // other major and an unspecified minor, which is not covered.
        if !is_major(opening) {
            s.add(
                "Michaels cuebid",
                &format!("^{OPEN}1{o} 2{o}$"),
                "Both majors",
                Forcing::Forcing,
                [fit(
                    MIN.with_hcp(8).min_len(Hearts, 5).min_len(Spades, 5),
                    MAX,
                )],
            );
        }
        let lowest_two = match opening {
            Clubs => MIN.min_len(Diamonds, 5).min_len(Hearts, 5),
            Diamonds => MIN.min_len(Clubs, 5).min_len(Hearts, 5),
            Hearts | Spades => MIN.min_len(Clubs, 5).min_len(Diamonds, 5),
        };
        s.add(
            "Unusual 2NT",
            &format!("^{OPEN}1{o} 2N$"),
            "The two lowest unbid suits",
            Forcing::Forcing,
            [fit(lowest_two.with_hcp(8), MAX)],
        );
        s.add(
            "1NT overcall",
            &format!("^{OPEN}1{o} 1N$"),
            "Balanced with a stopper",
            Forcing::Passable,
            [balanced_hcp(15, 18)],
        );
        for overcall in suits_except(&[opening]) {
            let level = cheapest_level(overcall, opening);
            let c = overcall.latin();
            let overcall_fit = longest(
                fit(
                    MIN.with_hcp(if level == 1 { 8 } else { 10 })
                        .min_len(overcall, 5),
                    MAX.with_hcp(16),
                ),
                overcall,
                suits_except(&[opening]),
            );
            s.add(
                "Overcall",
                &format!("^{OPEN}1{o} {level}{c}$"),
                "5+ card suit",
                Forcing::Passable,
                [overcall_fit.clone()],
            );
            s.add(
                "Balancing overcall",
                &format!("^{OPEN}1{o} P P {level}{c}$"),
                "5+ card suit",
                Forcing::Passable,
                [overcall_fit],
            );
        }
        // Support for the unbid suits, or a hand too strong to overcall.
        let support = suits_except(&[opening]).fold(MIN.with_hcp(12), |min, z| min.min_len(z, 3));
        s.add(
            "Takeout double",
            &format!("^{OPEN}1{o} X$"),
            "Support for the unbid suits, or 17+",
            Forcing::Forcing,
            [
                fit(support, MAX.max_len(opening, 3)),
                fit(MIN.with_hcp(17), MAX),
            ],
        );
        // Advancing a takeout double, which advancer may not pass for want
        // of a four card suit, so the cheapest suit is what is left when
        // nothing else fits.
        for advance in suits_except(&[opening]) {
            let level = cheapest_level(advance, opening);
            s.add(
                "Takeout double response",
                &format!("^{OPEN}1{o} X P {level}{}$", advance.latin()),
                "4+ card suit",
                Forcing::Passable,
                [fit(MIN.min_len(advance, 4), MAX)],
            );
        }
        let cheapest = Suit::ALL
            .iter()
            .cloned()
            .find(|&z| z > opening)
            .unwrap_or(Clubs);
        s.add(
            "Takeout double response",
            &format!(
                "^{OPEN}1{o} X P {}{}$",
                cheapest_level(cheapest, opening),
                cheapest.latin()
            ),
            "Nothing else fits, may be short",
            Forcing::Passable,
            [fit(MIN, MAX)],
        );
    }
}
