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
//! | 1NT over a major | 6-10 hcp, passable | 6-12 hcp, forcing |
//! | new suit at the two level | 10+ hcp, forcing one round | 12+ hcp, game forcing |
//! | opener's rebid after 1NT | pass with a minimum | must bid |
//!
//! A convention here is a table of regexes over the auction, each with
//! bounds that the bidder's hand must fit, and bidding picks the highest legal
//! bid that fits (see `ConventionalBid`).  A bid therefore has to be described
//! by a single box of hand bounds, which cannot say "more diamonds than clubs".
//! Where SAYC makes such a comparison the table approximates it (a comment
//! above `Convention` in `ai.rs` sketches changes that would remove these):
//!
//! * 1D shows four or more diamonds and at most four clubs, so 1C is also
//!   opened with 3-2 in the minors (the booklet opens 1D with 4-4-3-2), and
//!   5-5 or 6-5 in the minors opens 1C rather than 1D.
//! * With 5-5 in the majors 1S is opened, but 6-5 in hearts opens 1H.
//! * Over a minor opening, 4-4 in the majors responds 1S rather than 1H, and
//!   5-5 responds 1H rather than 1S.
//! * The booklet's 1NT over a major is 6-9, but here it is 6-10, so that a
//!   ten count with no suit to show still has a bid.  Its limit raise is 10-11
//!   with three or more trumps; here it is 10-12, since a 12 count with three
//!   trumps would otherwise need two bids (a new suit, then a raise).  For the
//!   same reason SAYC's Jacoby 2NT takes three trumps as well as four.
//! * In two over one the forcing 1NT over a major promises at most two cards
//!   in the major, since it is tried before the passable single raise.  The
//!   standard way to show a three card limit raise (1NT, then a jump in the
//!   major) needs two bids, so the direct limit raise 3M takes three card
//!   support.  After the forcing 1NT, opener's 2C is the bid that is left
//!   when nothing else fits, and may be short.
//!
//! Not covered, though the booklet has them: slam conventions (Blackwood and
//! Gerber), negative doubles, strong jump shifts, jump shifts by opener, a
//! 3NT opening and Michaels over a major.  (Splinters are not part of SAYC.)
//! Auctions the tables do not mention fall back on the natural bids shared
//! with the other conventions.
use super::{Convention, Forcing};
use bridge_deck::{HandValuation, Suit};
use display_as::{format_as, HTML};
use regex::RegexSet;

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
    fn add(
        &mut self,
        name: &'static str,
        regex: &str,
        description: &str,
        min: HandValuation,
        max: HandValuation,
        forcing: Forcing,
    ) {
        self.0.add(Convention::Simple {
            regex: RegexSet::new([regex]).unwrap(),
            the_description: format_as!(HTML, description),
            the_name: name,
            max,
            min,
            forcing,
        });
    }
}

const MIN: HandValuation = HandValuation::MIN;
const MAX: HandValuation = HandValuation::MAX;

/// The bounds of a balanced hand: no singleton or void and no six-card suit.
fn balanced() -> (HandValuation, HandValuation) {
    let mut min = MIN;
    let mut max = MAX;
    for suit in Suit::ALL.iter().cloned() {
        min = min.min_len(suit, 2);
        max = max.max_len(suit, 5);
    }
    (min, max)
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

fn standard_american(name: &'static str, two_over_one: bool) -> Convention {
    let mut s = Sheet(Convention::new(name));
    openings(&mut s);
    s.0.add_notrump((21, 22, 22), 8);
    s.0.add_strong_two_clubs(22);
    s.0.add_preempts();
    responses(&mut s, two_over_one);
    opener_rebids(&mut s, two_over_one);
    competition(&mut s);
    s.0.add_natural_bids();
    s.0
}

fn openings(s: &mut Sheet) {
    use Suit::*;
    // Hands of 13 to 21 points open one of a suit.
    let (min, max) = (MIN.with_lhcp(13), MAX.with_lhcp(21));
    let no_major = max.max_len(Hearts, 4).max_len(Spades, 4);

    s.add(
        "Opening bid",
        &format!("^{OPEN}1S$"),
        "5+ spades",
        min.min_len(Spades, 5),
        max.max_len(Hearts, 5),
        Forcing::Passable,
    );
    s.add(
        "Opening bid",
        &format!("^{OPEN}1H$"),
        "5+ hearts",
        min.min_len(Hearts, 5),
        max.max_len(Spades, 5),
        Forcing::Passable,
    );
    s.add(
        "Opening bid",
        &format!("^{OPEN}1D$"),
        "4+ diamonds, no 5 card major",
        min.min_len(Diamonds, 4),
        no_major.max_len(Clubs, 4),
        Forcing::Passable,
    );
    s.add(
        "Opening bid",
        &format!("^{OPEN}1C$"),
        "Better minor, no 5 card major",
        min.min_len(Clubs, 2),
        no_major,
        Forcing::Passable,
    );
    for major in [Hearts, Spades].iter().cloned() {
        s.add(
            "Preemptive opening",
            &format!("^{OPEN}4{}$", major.latin()),
            "8+ card suit",
            MIN.with_hcp(5).min_len(major, 8),
            MAX.with_hcp(11),
            Forcing::Passable,
        );
    }
    s.add(
        "Opening pass",
        &format!("^{OPEN}P$"),
        "",
        MIN,
        MAX.with_lhcp(12),
        Forcing::Passable,
    );
}

/// Responder's bids after a one-level opening, when the opponents pass or
/// double.
fn responses(s: &mut Sheet, two_over_one: bool) {
    use Suit::*;
    s.add(
        "Response pass",
        &format!("^{OPEN}1. P P$"),
        "",
        MIN,
        MAX.with_hcp(5),
        Forcing::Passable,
    );
    let (bal_min, bal_max) = balanced();
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        let after = format!("^{OPEN}1{o} [PX] ");
        let weak = MIN.with_hcp(6);
        // Responses that show a suit at the one level, up the line.
        for response in [Diamonds, Hearts, Spades]
            .iter()
            .cloned()
            .filter(|&r| r > opening)
        {
            let max = match response {
                Spades => MAX.max_len(Hearts, 4),
                // With a four card major, show it first.
                Diamonds => MAX.max_len(Hearts, 3).max_len(Spades, 3),
                _ => MAX,
            };
            s.add(
                "Response",
                &format!("{after}1{}$", response.latin()),
                "4+ cards",
                weak.min_len(response, 4),
                max,
                Forcing::Forcing,
            );
        }

        // 1NT.
        let mut max_nt = MAX.with_hcp(if two_over_one && is_major(opening) {
            12
        } else {
            10
        });
        if two_over_one && is_major(opening) {
            // The forcing notrump is tried before the passable single raise,
            // so it must leave three card support to the raise.
            max_nt = max_nt.max_len(opening, 2);
        }
        if opening == Hearts {
            max_nt = max_nt.max_len(Spades, 3);
        }
        if !is_major(opening) {
            max_nt = max_nt.max_len(Hearts, 3).max_len(Spades, 3);
        }
        if opening == Clubs {
            max_nt = max_nt.max_len(Diamonds, 3);
        }
        s.add(
            "Response",
            &format!("{after}1N$"),
            "No fit and no suit to bid",
            weak,
            max_nt,
            if two_over_one && is_major(opening) {
                Forcing::Forcing
            } else {
                Forcing::Passable
            },
        );

        if is_major(opening) {
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                "3+ card support",
                weak.min_len(opening, 3),
                MAX.with_hcp(9),
                Forcing::Passable,
            );
            s.add(
                "Limit raise",
                &format!("{after}3{o}$"),
                "3+ card support",
                MIN.with_hcp(10).min_len(opening, 3),
                MAX.with_hcp(12),
                Forcing::Passable,
            );
            // In SAYC a game force with three trumps has no other single
            // bid, since a new suit denies three card support.
            let jacoby_support = if two_over_one { 4 } else { 3 };
            s.add(
                "Jacoby 2NT",
                &format!("{after}2N$"),
                if two_over_one {
                    "4+ card support"
                } else {
                    "3+ card support"
                },
                MIN.with_hcp(13).min_len(opening, jacoby_support),
                MAX,
                Forcing::GameForcing,
            );
        } else {
            let no_major = MAX.max_len(Hearts, 3).max_len(Spades, 3);
            // Four trumps raise 1D, but five raise 1C.
            let (support, shown) = if opening == Diamonds {
                (4, "4+ card support")
            } else {
                (5, "5+ card support")
            };
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                shown,
                weak.min_len(opening, support),
                no_major.with_hcp(9),
                Forcing::Passable,
            );
            s.add(
                "Limit raise",
                &format!("{after}3{o}$"),
                shown,
                MIN.with_hcp(10).min_len(opening, support),
                no_major.with_hcp(12),
                Forcing::Passable,
            );
            s.add(
                "Response",
                &format!("{after}2N$"),
                "Balanced, no 4 card major",
                bal_min.with_hcp(13),
                bal_max.with_hcp(15).max_len(Hearts, 3).max_len(Spades, 3),
                Forcing::Forcing,
            );
        }

        // New suits at the two level.
        let (strength, forcing) = if two_over_one {
            (12, Forcing::GameForcing)
        } else {
            (10, Forcing::Forcing)
        };
        for response in Suit::ALL.iter().cloned().filter(|&r| r < opening) {
            let mut min = MIN.with_hcp(strength).min_len(response, 4);
            let mut max = MAX;
            if response == Hearts {
                min = min.min_len(Hearts, 5);
            }
            if opening == Hearts {
                // A four card spade suit is shown first.
                max = max.max_len(Spades, 3);
            }
            if !two_over_one && is_major(opening) {
                // With three trumps SAYC raises directly, and the new suit
                // is tried first because it is forcing.
                max = max.max_len(opening, 2);
            }
            if response == Diamonds {
                max = max.max_len(Clubs, 4);
            }
            if response == Clubs && opening == Diamonds {
                min = min.min_len(Clubs, 5);
            }
            s.add(
                "Two-level response",
                &format!("{after}2{}$", response.latin()),
                "New suit",
                min,
                max,
                forcing,
            );
        }
    }
}

/// Opener's second bid.
///
/// The booklet's ranges are a minimum of 13-15, a medium hand of 16-18
/// (jump raises and jump rebids, and reverses) and a non-reverse new suit of
/// 13-18.  After a one level response only a reverse forces, but after a two
/// level response responder has promised to bid again, so every rebid does.
fn opener_rebids(s: &mut Sheet, two_over_one: bool) {
    use Suit::*;
    let (bal_min, bal_max) = balanced();
    let min = MIN.with_lhcp(13);
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        for response in Suit::ALL.iter().cloned().filter(|&r| r != opening) {
            let level = cheapest_level(response, opening);
            let r = response.latin();
            let after = format!("^{OPEN}1{o} P {level}{r} P ");
            let rebid_forcing = if level == 2 {
                Forcing::Forcing
            } else {
                Forcing::Passable
            };
            // A four card major that can still be bid at the one level is
            // shown there rather than by a higher bid that would hide it.
            let hides_no_major = |max: HandValuation| {
                [Hearts, Spades]
                    .iter()
                    .cloned()
                    .filter(|&m| level == 1 && m != opening && m > response)
                    .fold(max, |max, m| max.max_len(m, 3))
            };
            // Both partners promised four cards, so opener needs four to raise.
            let support = |lhcp: u8| min.with_lhcp(lhcp).min_len(response, 4);
            s.add(
                "Rebid",
                &format!("{after}2{o}$"),
                "6+ card suit",
                min.min_len(opening, 6),
                hides_no_major(MAX.with_lhcp(15)),
                rebid_forcing,
            );
            s.add(
                "Jump rebid",
                &format!("{after}3{o}$"),
                "6+ card suit, invitational",
                MIN.with_lhcp(16).min_len(opening, 6),
                hides_no_major(MAX.with_lhcp(18)),
                rebid_forcing,
            );
            if level == 1 {
                s.add(
                    "Rebid",
                    &format!("{after}1N$"),
                    "Balanced minimum",
                    bal_min.with_hcp(13),
                    hides_no_major(bal_max.with_hcp(15)),
                    Forcing::Passable,
                );
                s.add(
                    "Jump rebid",
                    &format!("{after}2N$"),
                    "Balanced, 18-19 hcp",
                    bal_min.with_hcp(18),
                    hides_no_major(bal_max.with_hcp(19)),
                    Forcing::Passable,
                );
                s.add(
                    "Raise",
                    &format!("{after}2{r}$"),
                    "4+ card support",
                    support(13),
                    MAX.with_lhcp(15),
                    Forcing::Passable,
                );
                s.add(
                    "Raise",
                    &format!("{after}3{r}$"),
                    "4+ card support, invitational",
                    support(16),
                    MAX.with_lhcp(18),
                    Forcing::Passable,
                );
            } else {
                s.add(
                    "Rebid",
                    &format!("{after}2N$"),
                    "Balanced minimum",
                    bal_min.with_hcp(13),
                    bal_max.with_hcp(15),
                    rebid_forcing,
                );
                s.add(
                    "Raise",
                    &format!("{after}3{r}$"),
                    "Support",
                    min.min_len(response, if is_major(response) { 3 } else { 4 }),
                    MAX,
                    rebid_forcing,
                );
            }
            // A new suit, at the one level if it is cheap enough.  Bidding a
            // higher ranking suit than the opening at the two level is a
            // reverse.
            for z in Suit::ALL.iter().cloned() {
                if z == opening || z == response {
                    continue;
                }
                let zlevel = if z > response { level } else { level + 1 };
                let reverse = zlevel == 2 && z > opening;
                let (min, max, forcing) = if reverse {
                    let min = MIN.with_lhcp(16).min_len(opening, 5);
                    (min, MAX.with_lhcp(21), Forcing::Forcing)
                } else if zlevel == 2 {
                    (min, hides_no_major(MAX.with_lhcp(18)), rebid_forcing)
                } else {
                    (min, MAX.with_lhcp(21), rebid_forcing)
                };
                s.add(
                    "New suit",
                    &format!("{after}{zlevel}{}$", z.latin()),
                    if reverse { "Reverse" } else { "4+ cards" },
                    min.min_len(z, 4),
                    max,
                    forcing,
                );
            }
        }

        // After a 1NT response to a major, where opener's bids are not
        // forcing, though two over one's 1NT forces opener to make one.
        if is_major(opening) {
            let after = format!("^{OPEN}1{o} P 1N P ");
            s.add(
                "Rebid",
                &format!("{after}2{o}$"),
                "6+ card suit",
                min.min_len(opening, 6),
                MAX.with_lhcp(15),
                Forcing::Passable,
            );
            s.add(
                "Jump rebid",
                &format!("{after}3{o}$"),
                "6+ card suit, invitational",
                MIN.with_lhcp(16).min_len(opening, 6),
                MAX.with_lhcp(18),
                Forcing::Passable,
            );
            s.add(
                "Rebid",
                &format!("{after}2N$"),
                "Balanced, 18-19 hcp",
                bal_min.with_hcp(18),
                bal_max.with_hcp(19),
                Forcing::Passable,
            );
            for z in Suit::ALL.iter().cloned().filter(|&z| z != opening) {
                let regex = format!("{after}2{}$", z.latin());
                if two_over_one && z == Clubs {
                    // Opener may not pass the forcing notrump, so 2C is what
                    // is left when nothing else fits, and may be short.
                    s.0.add(Convention::Forced {
                        the_name: "Rebid over forcing 1NT",
                        regex: RegexSet::new([regex]).unwrap(),
                        min: MIN,
                        max: MAX,
                    });
                    continue;
                }
                let reverse = z > opening;
                s.add(
                    "New suit",
                    &regex,
                    if reverse { "Reverse" } else { "New suit" },
                    if reverse {
                        MIN.with_lhcp(16).min_len(opening, 5)
                    } else {
                        min
                    }
                    .min_len(z, 4),
                    MAX,
                    if reverse {
                        Forcing::Forcing
                    } else {
                        Forcing::Passable
                    },
                );
            }
        }
    }
}

/// Overcalls, takeout doubles and the two-suited overcalls.
fn competition(s: &mut Sheet) {
    use Suit::*;
    let (bal_min, bal_max) = balanced();
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        // The cheapest suit advancer can bid over partner's takeout double.
        let cheapest = Suit::ALL
            .iter()
            .cloned()
            .find(|&z| z > opening)
            .unwrap_or(Clubs);
        s.add(
            "Takeout double",
            &format!("^{OPEN}1{o} X$"),
            "Support for the unbid suits",
            Suit::ALL
                .iter()
                .cloned()
                .filter(|&z| z != opening)
                .fold(MIN.with_hcp(12), |min, z| min.min_len(z, 3)),
            MAX.max_len(opening, 3),
            Forcing::Forcing,
        );
        s.add(
            "1NT overcall",
            &format!("^{OPEN}1{o} 1N$"),
            "Balanced with a stopper",
            bal_min.with_hcp(15),
            bal_max.with_hcp(18),
            Forcing::Passable,
        );
        // Michaels over a minor shows both majors.  Over a major it shows the
        // other major and an unspecified minor, which a box cannot say.
        if !is_major(opening) {
            s.add(
                "Michaels cuebid",
                &format!("^{OPEN}1{o} 2{o}$"),
                "Both majors",
                MIN.with_hcp(8).min_len(Hearts, 5).min_len(Spades, 5),
                MAX,
                Forcing::Forcing,
            );
        }
        s.add(
            "Unusual 2NT",
            &format!("^{OPEN}1{o} 2N$"),
            "The two lowest unbid suits",
            match opening {
                Clubs => MIN.min_len(Diamonds, 5).min_len(Hearts, 5),
                Diamonds => MIN.min_len(Clubs, 5).min_len(Hearts, 5),
                Hearts | Spades => MIN.min_len(Clubs, 5).min_len(Diamonds, 5),
            }
            .with_hcp(8),
            MAX,
            Forcing::Forcing,
        );

        for overcall in Suit::ALL.iter().cloned().filter(|&z| z != opening) {
            let level = cheapest_level(overcall, opening);
            let min = MIN
                .with_hcp(if level == 1 { 8 } else { 10 })
                .min_len(overcall, 5);
            let c = overcall.latin();
            s.add(
                "Overcall",
                &format!("^{OPEN}1{o} {level}{c}$"),
                "5+ card suit",
                min,
                MAX.with_hcp(16),
                Forcing::Passable,
            );
            s.add(
                "Balancing overcall",
                &format!("^{OPEN}1{o} P P {level}{c}$"),
                "5+ card suit",
                min,
                MAX.with_hcp(16),
                Forcing::Passable,
            );
            // Advancing a takeout double, which advancer may not pass for
            // want of a four card suit, so the cheapest suit is what is left
            // when nothing else fits.
            let regex = format!("^{OPEN}1{o} X P {level}{c}$");
            if overcall == cheapest {
                s.0.add(Convention::Forced {
                    the_name: "Takeout double response",
                    regex: RegexSet::new([regex]).unwrap(),
                    min: MIN,
                    max: MAX,
                });
            } else {
                s.add(
                    "Takeout double response",
                    &regex,
                    "4+ card suit",
                    MIN.min_len(overcall, 4),
                    MAX,
                    Forcing::Passable,
                );
            }
        }
    }
}
