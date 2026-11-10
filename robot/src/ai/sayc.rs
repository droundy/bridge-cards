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
//! Natural games, slams and invitations come from the contract rules (see
//! `rules`), which come after the entries that describe a hand and before
//! the new suit responses to a major, which explore.  So SAYC's limit raise
//! of a major and its natural notrump responses are not written here, and
//! the single raise is a limit bid, whose most points the rules derive.
//! Raises count dummy points (`shcp`), as the booklet does.
//!
//! Where the table still differs from the booklet:
//!
//! * Notrump bids do not check for a stopper.
//! * 2H over 1S promises five hearts.
//! * In two over one the direct limit raise 3M takes three trumps, though
//!   the usual way to show three is 1NT and then a jump.
//!
//! Responder's second bid follows page 4 of the booklet (see
//! `responder_rebids`): a sign-off, which opener passes, an invitation,
//! which opener accepts with enough for 25 points, game, or a new suit,
//! which forces.  The notrump structure is SAYC's own (see `notrump`),
//! and answers 2NT after 2C-2D as it does a 2NT opening.  Stayman asks for
//! a four card major and shows nothing, though the robot asks only with a
//! four card major of its own, or 5-5 in the majors over 3NT.  The strong
//! 2C, the weak twos and threes, and the natural fallbacks are shared with
//! Sheets, though over a weak two's 2NT response opener rebids the suit
//! with a minimum.
//!
//! Blackwood and Gerber come from the contract rules (see `rules::slam`).
//!
//! As the booklet has it, a double by the opponents turns off the
//! conventions that answer the doubled bid, except over 1NT (see
//! `undoubled`), so that over a double of 2C, 2D is natural and positive.
//! Bids after a one-level response mean the same over a double, and the
//! contract rules take a double for a pass.  Responder answers a takeout
//! double of a one-level opening as the booklet does (see
//! `after_takeout_double`), with a weak jump shift, 2NT as a limit raise or
//! better, a preemptive jump raise and a redouble that shows 10+ points.
//!
//! Not covered, though the booklet has them: negative doubles, the feature
//! that opener shows over a weak two's 2NT with a maximum, and Michaels over
//! a major.  (Splinters are not part of SAYC.)  Auctions the tables do not
//! mention fall back on the natural bids shared with the other conventions.
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

/// A convention under construction: the entries that describe a hand, and
/// those that explore, which come after the contract rules, so that a hand
/// explores only when the rules do not know which contract to aim for.
struct Sheet(Convention, Convention);

/// An entry that a hand fits if it fits any of `fits`.
fn rule(
    name: &'static str,
    regex: &str,
    description: &str,
    forcing: Forcing,
    fits: impl IntoIterator<Item = Fit>,
) -> Convention {
    let fits: Vec<Fit> = fits.into_iter().collect();
    assert!(!fits.is_empty(), "{} {} fits no hand", name, regex);
    Convention::Rule {
        regex: RegexSet::new([regex]).unwrap(),
        the_description: format_as!(HTML, description),
        the_name: name,
        fits,
        forcing,
    }
}

impl Sheet {
    /// An entry that describes a hand that fits any of `fits`.
    fn add(
        &mut self,
        name: &'static str,
        regex: &str,
        description: &str,
        forcing: Forcing,
        fits: impl IntoIterator<Item = Fit>,
    ) {
        self.0.add(rule(name, regex, description, forcing, fits));
    }
    /// An entry that explores, for a hand that fits any of `fits`.
    fn explore(
        &mut self,
        name: &'static str,
        regex: &str,
        description: &str,
        forcing: Forcing,
        fits: impl IntoIterator<Item = Fit>,
    ) {
        self.1.add(rule(name, regex, description, forcing, fits));
    }
    /// The entries that `add` makes, which were written to be sorted the
    /// way Sheets is (see `Convention::most_forcing_then_highest`).
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
    let mut s = Sheet(Convention::new(name), Convention::new(""));
    // The notrump structure comes first, so that a balanced hand opens 2NT
    // or 3NT rather than 2C, and rebids 2NT over 2C-2D with 22-24 rather
    // than 3NT.
    notrump(&mut s);
    three_notrump(&mut s);
    s.add_block(|t| t.add_strong_two_clubs(22, false, true));
    s.add_block(|t| t.add_preempts());
    weak_two_rebids(&mut s);
    openings(&mut s);
    responses(&mut s, two_over_one);
    after_takeout_double(&mut s);
    opener_rebids(&mut s, two_over_one);
    responder_rebids(&mut s);
    competition(&mut s);
    // Natural invitations, games and slams when the opponents have only
    // passed or doubled, by the contract rules, then the bids that explore,
    // and the shared natural bids for when they have bid.
    s.0.add(Convention::Contract {
        the_name: "Natural",
        regex: RegexSet::new(["[1-7][CDHSN]$"]).unwrap(),
    });
    let explore = std::mem::replace(&mut s.1, Convention::new(""));
    s.0.append(explore);
    s.add_block(|t| t.add_natural_bids());
    s.0
}

/// `calls` separated by passes or doubles, after any passes, as a regex.
fn auction(calls: &[&str]) -> String {
    format!("^{OPEN}{}$", calls.join(" [PX] "))
}

/// `calls` as `auction` writes them, except that the opponent passes the
/// first: as the booklet has it, a double cancels a convention that
/// answers it, though not one that answers a doubled convention.
fn undoubled(calls: &[&str]) -> String {
    match calls.split_first() {
        Some((first, rest)) if !rest.is_empty() => {
            format!("^{OPEN}{first} P {}$", rest.join(" [PX] "))
        }
        _ => auction(calls),
    }
}

/// The notrump openings, and the booklet's responses to them in the order
/// responder considers them: Jacoby transfers, Stayman with a four card
/// major, the 2S relay to 3C with a weak minor, invitations, game and the
/// quantitative 4NT, with the continuations the booklet gives.
fn notrump(s: &mut Sheet) {
    use Suit::*;
    // Each opening, with responder's points to invite (if 1NT) and for
    // game, as the contract rules give them: over 2NT there is no room to
    // invite, so game takes 25 opposite 21.  The natural notrump bids
    // themselves come from the rules.  The booklet answers 2NT after 2C-2D,
    // which shows 22-24, as it answers a 2NT opening.
    for (nt, (lo, hi), invite, game) in [
        ("2N", (20, 21), None, 4),
        ("2C (P 2D|X P) [PX] 2N", (22, 24), None, 3),
        ("1N", (15, 17), Some((8, 9)), 10),
    ] {
        // Over a double the booklet keeps the conventions on after 1NT, but
        // not after the others.
        let auction = |calls: &[&str]| {
            if nt == "1N" {
                auction(calls)
            } else {
                undoubled(calls)
            }
        };
        let name = match nt {
            "1N" => "Opening 1NT",
            "2N" => "Opening 2NT",
            _ => "Strong two rebid",
        };
        s.add(
            name,
            &auction(&[nt]),
            "",
            Forcing::Passable,
            [balanced_hcp(lo, hi)],
        );
        let level = if nt == "1N" { 2 } else { 3 };
        let call = |strain: char| format!("{level}{strain}");
        let (stayman, to_hearts, to_spades) = (call('C'), call('D'), call('H'));
        // The top two points of 1NT's range, and the top point of 2NT's.
        let maximum = MIN.with_hcp(if nt == "1N" { hi - 1 } else { hi });
        let stayman_min = invite.map_or(game, |(lo, _)| lo);

        // Responder's first bid.  The booklet gives 1NT-3M no points, so it
        // takes what makes a small slam possible opposite 1NT's maximum.
        if nt == "1N" {
            let slam_interest = MIN.with_lhcp(33 - hi);
            s.add(
                "Response",
                &auction(&[nt, "3S"]),
                "6+ spades, slam interest",
                Forcing::GameForcing,
                [fit(slam_interest.min_len(Spades, 6), MAX).with(AtLeastAsLong(Spades, Hearts))],
            );
            s.add(
                "Response",
                &auction(&[nt, "3H"]),
                "6+ hearts, slam interest",
                Forcing::GameForcing,
                [fit(slam_interest.min_len(Hearts, 6), MAX)],
            );
        }
        s.add(
            "Jacoby transfer",
            &auction(&[nt, &to_spades]),
            "5+ spades",
            Forcing::Forcing,
            [fit(MIN.min_len(Spades, 5), MAX).with(AtLeastAsLong(Spades, Hearts))],
        );
        s.add(
            "Jacoby transfer",
            &auction(&[nt, &to_hearts]),
            "5+ hearts",
            Forcing::Forcing,
            [fit(MIN.min_len(Hearts, 5), MAX)],
        );
        s.add(
            "Stayman",
            &auction(&[nt, &stayman]),
            "Asks for a 4 card major",
            Forcing::Forcing,
            [
                fit(MIN.with_hcp(stayman_min).min_len(Hearts, 4), MAX).asks(),
                fit(MIN.with_hcp(stayman_min).min_len(Spades, 4), MAX).asks(),
            ],
        );
        if let Some((invite_lo, invite_hi)) = invite {
            let weak = MAX.with_hcp(invite_lo - 1);
            s.add(
                "Minor relay",
                &auction(&[nt, "2S"]),
                "Weak, with a 6+ card minor",
                Forcing::Forcing,
                [
                    fit(MIN.min_len(Clubs, 6), weak),
                    fit(MIN.min_len(Diamonds, 6), weak),
                ],
            );
            for minor in [Diamonds, Clubs] {
                s.add(
                    "Response",
                    &auction(&[nt, &format!("3{}", minor.latin())]),
                    "6+ cards, invitational",
                    Forcing::Passable,
                    [fit(
                        MIN.with_hcp(invite_lo).min_len(minor, 6),
                        MAX.with_hcp(invite_hi),
                    )],
                );
            }
        }
        s.add(
            "Response pass",
            &auction(&[nt, "P"]),
            "Too weak to invite",
            Forcing::Passable,
            [fit(MIN, MAX.with_hcp(stayman_min - 1))],
        );

        // Opener's replies.
        s.add(
            "Stayman response",
            &auction(&[nt, &stayman, &call('H')]),
            "4+ hearts",
            Forcing::Passable,
            [fit(MIN.min_len(Hearts, 4), MAX)],
        );
        s.add(
            "Stayman response",
            &auction(&[nt, &stayman, &call('S')]),
            "4+ spades, not 4 hearts",
            Forcing::Passable,
            [fit(MIN.min_len(Spades, 4), MAX.max_len(Hearts, 3))],
        );
        s.add(
            "Stayman response",
            &auction(&[nt, &stayman, &call('D')]),
            "No 4 card major",
            Forcing::Passable,
            [fit(MIN, MAX.max_len(Hearts, 3).max_len(Spades, 3))],
        );
        for (transfer, major) in [(&to_hearts, Hearts), (&to_spades, Spades)] {
            let accept = call(major.latin());
            if nt == "1N" {
                s.add(
                    "Transfer accepted",
                    &auction(&[nt, transfer, &format!("3{}", major.latin())]),
                    "17 points and 4 card support",
                    Forcing::Passable,
                    [fit(MIN.with_hcp(17).min_len(major, 4), MAX)],
                );
            }
            s.add(
                "Transfer accepted",
                &auction(&[nt, transfer, &accept]),
                "",
                Forcing::Passable,
                [fit(MIN, MAX)],
            );
        }
        if invite.is_some() {
            s.add(
                "Minor relay",
                &auction(&[nt, "2S", "3C"]),
                "Forced",
                Forcing::Passable,
                [fit(MIN, MAX)],
            );
        }

        // Responder's second bid.
        let invitational = invite.map(|(lo, hi)| (MIN.with_hcp(lo), MAX.with_hcp(hi)));
        let game_values = MIN.with_hcp(game);
        if invite.is_some() {
            s.add(
                "Minor relay",
                &auction(&[nt, "2S", "3C", "3D"]),
                "Diamonds",
                Forcing::Passable,
                [fit(MIN.min_len(Diamonds, 6), MAX)],
            );
        }
        for reply in ['D', 'H', 'S'] {
            let reply = call(reply);
            let fit_in = match reply.as_bytes()[1] {
                b'H' => Some(Hearts),
                b'S' => Some(Spades),
                _ => None,
            };
            let after = |bid: &str| auction(&[nt, &stayman, &reply, bid]);
            if let Some(major) = fit_in {
                let m = major.latin();
                s.add(
                    "Raise",
                    &after(&format!("4{m}")),
                    "4 card support, game",
                    Forcing::Passable,
                    [fit(game_values.min_len(major, 4), MAX)],
                );
                if let Some((min, max)) = invitational {
                    s.add(
                        "Raise",
                        &after(&format!("3{m}")),
                        "4 card support, invitational",
                        Forcing::Passable,
                        [fit(min.min_len(major, 4), max)],
                    );
                }
            }
            s.add(
                "Game",
                &after("3N"),
                "",
                Forcing::Passable,
                [fit(game_values, MAX)],
            );
            if let Some((min, max)) = invitational {
                s.add(
                    "Game invite",
                    &after("2N"),
                    "",
                    Forcing::Passable,
                    [fit(min, max)],
                );
            }
        }
        for (transfer, major) in [(&to_hearts, Hearts), (&to_spades, Spades)] {
            let m = major.latin();
            let accept = call(m);
            let after = |bid: &str| auction(&[nt, transfer, &accept, bid]);
            s.add(
                "Game",
                &after(&format!("4{m}")),
                "6+ cards",
                Forcing::Passable,
                [fit(game_values.min_len(major, 6), MAX)],
            );
            s.add(
                "Game",
                &after("3N"),
                "5 cards, a choice of games",
                Forcing::Passable,
                [fit(game_values, MAX.max_len(major, 5))],
            );
            if let Some((min, max)) = invitational {
                s.add(
                    "Game invite",
                    &after(&format!("3{m}")),
                    "6+ cards",
                    Forcing::Passable,
                    [fit(min.min_len(major, 6), max)],
                );
                s.add(
                    "Game invite",
                    &after("2N"),
                    "5 cards",
                    Forcing::Passable,
                    [fit(min, max.max_len(major, 5))],
                );
            }
        }

        // Opener's third bid.
        for major in [Hearts, Spades] {
            let m = major.latin();
            let reply = call(m);
            if invite.is_some() {
                s.add(
                    "Game",
                    &auction(&[nt, &stayman, &reply, &format!("3{m}"), &format!("4{m}")]),
                    "Maximum",
                    Forcing::Passable,
                    [fit(maximum, MAX)],
                );
            }
        }
        // Responder's notrump after 2H may hide four spades, so opener with
        // four of them bids them.
        s.add(
            "Game",
            &auction(&[nt, &stayman, &call('H'), "3N", "4S"]),
            "4 spades",
            Forcing::Passable,
            [fit(MIN.min_len(Spades, 4), MAX)],
        );
        if invite.is_some() {
            let after_invite = |bid: &str| auction(&[nt, &stayman, &call('H'), "2N", bid]);
            s.add(
                "Game",
                &after_invite("4S"),
                "Maximum, 4 spades",
                Forcing::Passable,
                [fit(maximum.min_len(Spades, 4), MAX)],
            );
            s.add(
                "Game",
                &after_invite("3N"),
                "Maximum",
                Forcing::Passable,
                [fit(maximum, MAX)],
            );
            s.add(
                "Rebid",
                &after_invite("3S"),
                "Minimum, 4 spades",
                Forcing::Passable,
                [fit(MIN.min_len(Spades, 4), MAX)],
            );
            for reply in ['D', 'S'] {
                s.add(
                    "Game",
                    &auction(&[nt, &stayman, &call(reply), "2N", "3N"]),
                    "Maximum",
                    Forcing::Passable,
                    [fit(maximum, MAX)],
                );
            }
        }
        for (transfer, major) in [(&to_hearts, Hearts), (&to_spades, Spades)] {
            let m = major.latin();
            let accepted = |bid: &str, then: &str| auction(&[nt, transfer, &call(m), bid, then]);
            s.add(
                "Game",
                &accepted("3N", &format!("4{m}")),
                "3+ card support",
                Forcing::Passable,
                [fit(MIN.min_len(major, 3), MAX)],
            );
            if invite.is_some() {
                s.add(
                    "Game",
                    &accepted(&format!("3{m}"), &format!("4{m}")),
                    "Maximum",
                    Forcing::Passable,
                    [fit(maximum, MAX)],
                );
                s.add(
                    "Game",
                    &accepted("2N", &format!("4{m}")),
                    "Maximum, 3+ card support",
                    Forcing::Passable,
                    [fit(maximum.min_len(major, 3), MAX)],
                );
                s.add(
                    "Game",
                    &accepted("2N", "3N"),
                    "Maximum, 2 card support",
                    Forcing::Passable,
                    [fit(maximum, MAX)],
                );
                s.add(
                    "Rebid",
                    &accepted("2N", &format!("3{m}")),
                    "Minimum, 3+ card support",
                    Forcing::Passable,
                    [fit(MIN.min_len(major, 3), MAX)],
                );
            }
        }
    }
}

/// Opener's rebid of a weak two's suit over 2NT, which the booklet makes
/// forcing, with a minimum of 5-8 points.  With more, the contract rules
/// answer 2NT as the invitation they make it.
fn weak_two_rebids(s: &mut Sheet) {
    for suit in [Suit::Diamonds, Suit::Hearts, Suit::Spades] {
        let two = format!("2{}", suit.latin());
        s.add(
            "Weak two rebid",
            &auction(&[&two, "2N", &format!("3{}", suit.latin())]),
            "A minimum",
            Forcing::Passable,
            [fit(MIN, MAX.with_hcp(8))],
        );
    }
}

/// The 3NT opening, which comes before the strong 2C that its hands also
/// fit, with the booklet's Stayman and transfers at the four level, which a
/// double turns off.  The
/// quantitative 4NT and the slams come from the contract rules.
fn three_notrump(s: &mut Sheet) {
    use Suit::*;
    let (lo, hi) = (25, 27);
    s.add(
        "Opening 3NT",
        &undoubled(&["3N"]),
        "",
        Forcing::Passable,
        [balanced_hcp(lo, hi)],
    );
    // Game is already bid, so Stayman looks for a slam in a major, with the
    // points that make one possible opposite 3NT's maximum.  After 4D there
    // is no way back to 3NT, and 4NT is then quantitative.  With 5-5 in the
    // majors it finds the eight card fit that 3NT's balanced hand must have
    // in one of them, at any strength, which a transfer might miss.
    let five_five = MIN.min_len(Hearts, 5).min_len(Spades, 5);
    s.add(
        "Stayman",
        &undoubled(&["3N", "4C"]),
        "Asks for a 4 card major",
        Forcing::Forcing,
        [fit(five_five, MAX).asks()],
    );
    s.add(
        "Jacoby transfer",
        &undoubled(&["3N", "4H"]),
        "5+ spades",
        Forcing::Forcing,
        [fit(MIN.min_len(Spades, 5), MAX).with(AtLeastAsLong(Spades, Hearts))],
    );
    s.add(
        "Jacoby transfer",
        &undoubled(&["3N", "4D"]),
        "5+ hearts",
        Forcing::Forcing,
        [fit(MIN.min_len(Hearts, 5), MAX)],
    );
    let slam_possible = MIN.with_hcp(33 - hi);
    s.add(
        "Stayman",
        &undoubled(&["3N", "4C"]),
        "Asks for a 4 card major",
        Forcing::Forcing,
        [
            fit(slam_possible.min_len(Hearts, 4), MAX).asks(),
            fit(slam_possible.min_len(Spades, 4), MAX).asks(),
        ],
    );
    s.add(
        "Response pass",
        &undoubled(&["3N", "P"]),
        "No slam interest",
        Forcing::Passable,
        [fit(MIN, MAX.with_hcp(33 - hi - 1))],
    );

    // Opener's replies.
    s.add(
        "Stayman response",
        &undoubled(&["3N", "4C", "4H"]),
        "4+ hearts",
        Forcing::Passable,
        [fit(MIN.min_len(Hearts, 4), MAX)],
    );
    s.add(
        "Stayman response",
        &undoubled(&["3N", "4C", "4S"]),
        "4+ spades, not 4 hearts",
        Forcing::Passable,
        [fit(MIN.min_len(Spades, 4), MAX.max_len(Hearts, 3))],
    );
    s.add(
        "Stayman response",
        &undoubled(&["3N", "4C", "4D"]),
        "No 4 card major",
        Forcing::Passable,
        [fit(MIN, MAX.max_len(Hearts, 3).max_len(Spades, 3))],
    );
    for (transfer, accept) in [("4D", "4H"), ("4H", "4S")] {
        s.add(
            "Transfer accepted",
            &undoubled(&["3N", transfer, accept]),
            "",
            Forcing::Passable,
            [fit(MIN, MAX)],
        );
    }

    // With 5-5 after 4D, responder lets opener choose the major, which the
    // booklet does not say.
    s.add(
        "Pass or correct",
        &undoubled(&["3N", "4C", "4D", "4H"]),
        "5-5 in the majors",
        Forcing::Passable,
        [fit(five_five, MAX)],
    );
    s.add(
        "Choice of games",
        &undoubled(&["3N", "4C", "4D", "4H", "P"]),
        "3 hearts",
        Forcing::Passable,
        [fit(MIN.min_len(Hearts, 3), MAX)],
    );
    s.add(
        "Choice of games",
        &undoubled(&["3N", "4C", "4D", "4H", "4S"]),
        "2 hearts, so 3 spades",
        Forcing::Passable,
        [fit(MIN.min_len(Spades, 3), MAX.max_len(Hearts, 2))],
    );
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

/// Responder's new suit at the one level, `response` over 1`opening`,
/// which forces: up the line with 4-4, the longer with 5-4, and spades
/// with 5-5.
fn one_level_response(after: &str, opening: Suit, response: Suit) -> Convention {
    use Suit::*;
    let four = MIN.with_hcp(6).min_len(response, 4);
    let (shown, fits) = match response {
        Spades if !is_major(opening) => (
            "4+ spades",
            vec![
                fit(four, MAX).with(Longer(Spades, Hearts)),
                fit(four.min_len(Spades, 5), MAX).with(AtLeastAsLong(Spades, Hearts)),
            ],
        ),
        Spades => ("4+ spades", vec![fit(four, MAX)]),
        Hearts => ("4+ hearts", vec![fit(four, MAX)]),
        _ => ("4+ diamonds", vec![fit(four, MAX)]),
    };
    let regex = format!("{after}1{}$", response.latin());
    rule("Response", &regex, shown, Forcing::Forcing, fits)
}

/// Responder's bids after a one-level opening, when the opponents pass.
fn responses(s: &mut Sheet, two_over_one: bool) {
    use Suit::*;
    let weak = MIN.with_hcp(6);
    let no_major = MAX.max_len(Hearts, 3).max_len(Spades, 3);
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        let after = format!("^{OPEN}1{o} P ");
        // New suits at the two level, the longest first, which explore if
        // `explore`.
        let two_level = |s: &mut Sheet, explore: bool| {
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
                let entry = rule(
                    "Two-level response",
                    &format!("{after}2{}$", response.latin()),
                    "New suit",
                    forcing,
                    [longest(fit(min, MAX), response, suits_except(&[opening]))],
                );
                if explore {
                    s.1.add(entry);
                } else {
                    s.0.add(entry);
                }
            }
        };

        // The booklet's strong jump shifts, which invite a slam, and give no
        // points or shape.  They take the 18 points that make a slam
        // possible opposite an opening and two more points, a five card
        // suit, the longest, and at most three cards in opener's suit, since
        // Jacoby 2NT shows four.  After them the contract rules explore.
        let jump_shifts = |s: &mut Sheet| {
            for response in suits_except(&[opening]) {
                let level = cheapest_level(response, opening) + 1;
                s.add(
                    "Jump shift",
                    &format!("{after}{level}{}$", response.latin()),
                    "5+ cards, slam interest",
                    Forcing::GameForcing,
                    [longest(
                        fit(
                            MIN.with_lhcp(18).min_len(response, 5),
                            MAX.max_len(opening, 3),
                        ),
                        response,
                        suits_except(&[opening]),
                    )],
                );
            }
        };

        if is_major(opening) {
            jump_shifts(s);
            s.add(
                "Jacoby 2NT",
                &format!("{after}2N$"),
                "4+ card support",
                Forcing::GameForcing,
                [fit(MIN.with_shcp(13).min_len(opening, 4), MAX)],
            );
            // The booklet's 4M is "usually" five trumps and a singleton or
            // void with fewer than 10 HCP; here it is always that, and short
            // of Jacoby's dummy points.
            let preempt = MAX.with_hcp(9).with_shcp(12);
            s.add(
                "Preemptive raise",
                &format!("{after}4{o}$"),
                "5+ card support, a singleton or void",
                Forcing::Passable,
                suits_except(&[opening])
                    .map(|short| fit(MIN.min_len(opening, 5), preempt.max_len(short, 1))),
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
            // SAYC's limit raise is the contract rules' invitation, but two
            // over one's takes a twelfth point, short of its game force.
            if two_over_one {
                s.add(
                    "Limit raise",
                    &format!("{after}3{o}$"),
                    "3+ card support",
                    Forcing::Passable,
                    [fit(
                        MIN.with_shcp(10).min_len(opening, 3),
                        MAX.with_shcp(12),
                    )],
                );
            }
            // New suits explore, after the raises and the contract rules.
            if opening == Hearts {
                s.1.add(one_level_response(&after, opening, Spades));
            }
            two_level(s, true);
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                "3+ card support",
                Forcing::Passable,
                [fit(MIN.with_shcp(6).min_len(opening, 3), MAX).limit()],
            );
            // It denies four spades, which bid 1S over 1H.  Two over one's
            // forcing 1NT explores, after the new suits, as before.
            let notrump = rule(
                "Response",
                &format!("{after}1N$"),
                "No fit and no suit to bid",
                if two_over_one {
                    Forcing::Forcing
                } else {
                    Forcing::Passable
                },
                [fit(
                    weak,
                    MAX.with_hcp(if two_over_one { 12 } else { 9 })
                        .max_len(Spades, if opening == Hearts { 3 } else { 13 }),
                )],
            );
            if two_over_one {
                s.1.add(notrump);
            } else {
                s.0.add(notrump);
            }
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
            jump_shifts(s);
            // A major before a raise of the minor: up the line with 4-4, the
            // longer with 5-4, and spades with 5-5.
            for response in suits_except(&[opening]).filter(|&r| r > opening) {
                s.0.add(one_level_response(&after, opening, response));
            }
            two_level(s, false);
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
                    MIN.with_shcp(10).min_len(opening, support),
                    MAX.with_shcp(12),
                )],
            );
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                shown,
                Forcing::Passable,
                [fit(MIN.with_shcp(6).min_len(opening, support), MAX).limit()],
            );
            // Up to the strength of a new suit at the two level, which in
            // two over one is a game force.
            s.add(
                "Response",
                &format!("{after}1N$"),
                "No fit and no suit to bid",
                Forcing::Passable,
                [fit(weak, MAX.with_hcp(if two_over_one { 11 } else { 10 }))],
            );
        }
    }
    // Responder may not pass with six or more points, so when nothing else
    // fits, after the contract rules too, there is a forcing bid that may be
    // short.  With less than its strength, 1NT has already fit.
    for opening in Suit::ALL.iter().cloned() {
        let after = format!("^{OPEN}1{} P ", opening.latin());
        if opening == Clubs {
            s.explore(
                "Response",
                &format!("{after}1D$"),
                "Nothing else fits, may be short",
                Forcing::Forcing,
                [fit(weak, MAX)],
            );
        } else {
            s.explore(
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
    }
    s.add(
        "Response pass",
        &format!("^{OPEN}1[CDHS] P P$"),
        "",
        Forcing::Passable,
        [fit(MIN, MAX.with_hcp(5))],
    );
}

/// Responder's bids after the opponents double a one-level opening for
/// takeout, as the booklet gives them over 1D, in two over one too: a jump
/// shift is weak, a new suit at the one level forces, 2NT is a limit
/// raise or better and a jump raise is preemptive.  A redouble shows 10+
/// points, after the bids that describe the hand better, and so the single
/// raise, a new suit at the two level and 1NT show less.  Then opener
/// passes, and a double of the opponents' escape is for penalty.
fn after_takeout_double(s: &mut Sheet) {
    use Suit::*;
    for opening in Suit::ALL.iter().cloned() {
        let o = opening.latin();
        let after = format!("^{OPEN}1{o} X ");
        // The trumps that raise: three for a major, four for 1D, five for 1C.
        let support = match opening {
            Clubs => 5,
            Diamonds => 4,
            _ => 3,
        };
        let (shown, more) = match support {
            3 => ("3+ card support", "4+ card support"),
            4 => ("4+ card support", "5+ card support"),
            _ => ("5+ card support", "6+ card support"),
        };
        let weak = MAX.with_hcp(9);
        let jump_shifts = |s: &mut Sheet| {
            for response in suits_except(&[opening]) {
                let level = cheapest_level(response, opening) + 1;
                s.add(
                    "Weak jump shift",
                    &format!("{after}{level}{}$", response.latin()),
                    if level == 2 {
                        "6+ cards, to play"
                    } else {
                        "7+ cards, to play"
                    },
                    Forcing::Passable,
                    [longest(
                        fit(
                            MIN.with_hcp(5).min_len(response, level as u8 + 4),
                            weak.max_len(opening, support - 1),
                        ),
                        response,
                        suits_except(&[opening]),
                    )],
                );
            }
        };
        let raises = |s: &mut Sheet| {
            s.add(
                "Limit raise or better",
                &format!("{after}2N$"),
                shown,
                Forcing::Forcing,
                [fit(MIN.with_shcp(10).min_len(opening, support), MAX)],
            );
            if is_major(opening) {
                let preempt = MAX.with_hcp(9).with_shcp(12);
                s.add(
                    "Preemptive raise",
                    &format!("{after}4{o}$"),
                    "5+ card support, a singleton or void",
                    Forcing::Passable,
                    suits_except(&[opening])
                        .map(|short| fit(MIN.min_len(opening, 5), preempt.max_len(short, 1))),
                );
            }
            s.add(
                "Preemptive raise",
                &format!("{after}3{o}$"),
                more,
                Forcing::Passable,
                [fit(MIN.min_len(opening, support + 1), MAX.with_shcp(9))],
            );
        };
        let single_raise = |s: &mut Sheet| {
            s.add(
                "Single raise",
                &format!("{after}2{o}$"),
                shown,
                Forcing::Passable,
                [fit(MIN.with_shcp(6).min_len(opening, support), MAX).limit()],
            );
        };
        // As without the double, a major is raised before a new suit, but a
        // minor after a major.
        jump_shifts(s);
        if is_major(opening) {
            raises(s);
            single_raise(s);
        }
        for response in suits_except(&[opening]).filter(|&r| r > opening) {
            s.0.add(one_level_response(&after, opening, response));
        }
        if !is_major(opening) {
            raises(s);
        }
        s.add(
            "Redouble",
            &format!("{after}XX$"),
            "10+ points",
            Forcing::Passable,
            [fit(MIN.with_hcp(10), MAX)],
        );
        if !is_major(opening) {
            single_raise(s);
        }
        for response in suits_except(&[opening]).filter(|&r| r < opening) {
            s.add(
                "Two-level response",
                &format!("{after}2{}$", response.latin()),
                "6+ cards, not forcing",
                Forcing::Passable,
                [fit(MIN.with_hcp(6).min_len(response, 6), MAX.with_hcp(10))],
            );
        }
        s.add(
            "Response",
            &format!("{after}1N$"),
            "No fit and no suit to bid",
            Forcing::Passable,
            [fit(
                MIN.with_hcp(6),
                weak.max_len(Spades, if opening == Hearts { 3 } else { 13 }),
            )],
        );
        s.add(
            "Response pass",
            &format!("{after}P$"),
            "Nothing else fits",
            Forcing::Passable,
            [fit(MIN, weak)],
        );
        // After the redouble opener passes, to defend if they do not escape,
        // and their escape is doubled for penalty, by either of us, holding
        // four of their trumps.
        s.add(
            "Pass",
            &format!("{after}XX P P$"),
            "Defends",
            Forcing::Passable,
            [fit(MIN, MAX)],
        );
        for escape in Suit::ALL.iter().cloned() {
            s.add(
                "Penalty double",
                &format!("{after}XX (P P )?[1-4]{}( P P)? X$", escape.latin()),
                "4+ cards in their suit",
                Forcing::Passable,
                [fit(MIN.min_len(escape, 4), MAX)],
            );
        }
    }
}

/// The opponent's call between the opening and a new suit response at
/// `level`, for the bids after it: a one-level response means the same
/// over a double, but a two-level one does not.
fn over_double(level: usize) -> &'static str {
    if level == 1 {
        "[PX]"
    } else {
        "P"
    }
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
            let after = format!("^{OPEN}1{o} {} {level}{r} P ", over_double(level));
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

            // With a maximum, 19-21, opener jumps to game in a major or
            // jumps in a new suit, forcing to game.
            if level == 1 {
                let maximum = MIN.with_lhcp(19);
                if is_major(response) {
                    s.add(
                        "Raise",
                        &format!("{after}4{r}$"),
                        "4+ card support, maximum",
                        Forcing::Passable,
                        [fit(maximum.min_len(response, 4), MAX)],
                    );
                }
                if is_major(opening) {
                    s.add(
                        "Jump rebid",
                        &format!("{after}4{o}$"),
                        "6+ card suit, maximum",
                        Forcing::Passable,
                        [fit(maximum.min_len(opening, 6), MAX)],
                    );
                }
                for &(zlevel, z, _) in new_suits.iter() {
                    s.add(
                        "Jump shift",
                        &format!("{after}{}{}$", zlevel + 1, z.latin()),
                        "4+ cards, maximum",
                        Forcing::GameForcing,
                        [longest(
                            fit(maximum.min_len(z, 4), MAX),
                            z,
                            new_suits.iter().map(|&(_, other, _)| other),
                        )],
                    );
                }
            }
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
        // forcing, though two over one's 1NT forces opener to make one
        // unless it answered a double.
        if is_major(opening) {
            let after = format!("^{OPEN}1{o} [PX] 1N P ");
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
                    &format!("^{OPEN}1{o} P 1N P 2C$"),
                    "Nothing else fits, may be short",
                    Forcing::Passable,
                    [fit(MIN, MAX)],
                );
            }
        }
    }
}

/// What a rebid by opener showed, for responder's next bid.
struct Shown {
    level: usize,
    /// The suit named, or `None` for notrump.
    strain: Option<Suit>,
    /// Opener's points, high card points with length.
    lo: u8,
    hi: u8,
    /// The cards shown in `strain`.
    length: u8,
    /// Must responder bid again?
    forcing: bool,
}

impl Shown {
    fn call(&self) -> String {
        call(self.level, self.strain)
    }
}

/// A bid as `bids_string` writes it.
fn call(level: usize, strain: Option<Suit>) -> String {
    match strain {
        Some(suit) => format!("{level}{}", suit.latin()),
        None => format!("{level}N"),
    }
}

/// Is the bid at `level` in `strain` higher than `than`?
fn above(level: usize, strain: Option<Suit>, than: &Shown) -> bool {
    let rank = |strain: Option<Suit>| strain.map_or(4, |s| s as usize);
    (level, rank(strain)) > (than.level, rank(than.strain))
}

/// Opener's rebids after `1{opening} P {level}{response} P`, as
/// `opener_rebids` writes them.
fn opener_rebids_shown(opening: Suit, response: Suit) -> Vec<Shown> {
    let level = cheapest_level(response, opening);
    let shown = |level, strain, (lo, hi), length, forcing| Shown {
        level,
        strain,
        lo,
        hi,
        length,
        forcing,
    };
    // After a two level response, responder promises to bid again.
    let two = level == 2;
    let mut rebids = Vec::new();
    for z in suits_except(&[opening, response]) {
        let zlevel = if z > response { level } else { level + 1 };
        if zlevel == 2 && z > opening {
            rebids.push(shown(zlevel, Some(z), (16, 21), 4, true));
        } else {
            rebids.push(shown(zlevel, Some(z), (13, 18), 4, two));
            if level == 1 && zlevel < 3 {
                rebids.push(shown(zlevel + 1, Some(z), (19, 21), 4, true));
            }
        }
    }
    let raise = if is_major(response) { 4 } else { 5 };
    rebids.push(shown(2, Some(opening), (13, 15), 5, two));
    rebids.push(shown(3, Some(opening), (16, 18), 6, two));
    if level == 1 {
        rebids.push(shown(1, None, (13, 15), 0, false));
        rebids.push(shown(2, None, (18, 19), 0, false));
        rebids.push(shown(2, Some(response), (13, 15), raise, false));
        rebids.push(shown(3, Some(response), (16, 18), raise, false));
    } else {
        rebids.push(shown(2, None, (13, 15), 0, true));
        rebids.push(shown(3, Some(response), (13, 18), raise, true));
    }
    rebids
}

/// Responder's second bid, after opener's rebid, and opener's answer to an
/// invitation.  As the booklet has it: pass, 1NT or two of a suit already
/// bid to sign off; 2NT or three of a suit already bid to invite game;
/// game with more; and a new suit, which forces opener to bid again.  The
/// partnership needs 25 points for game.
fn responder_rebids(s: &mut Sheet) {
    for opening in Suit::ALL.iter().cloned() {
        for response in suits_except(&[opening]) {
            let level = cheapest_level(response, opening);
            let first = format!(
                "^{OPEN}1{} {} {level}{} P ",
                opening.latin(),
                over_double(level),
                response.latin()
            );
            for x in opener_rebids_shown(opening, response) {
                responder_after(s, &first, opening, response, &x);
            }
        }
    }
}

/// Responder's bids after opener's rebid `x`, where `first` is the auction
/// up to it.
fn responder_after(s: &mut Sheet, first: &str, opening: Suit, response: Suit, x: &Shown) {
    let after = format!("{first}{} P ", x.call());
    let regex = |bid: &str| format!("{after}{bid}$");
    let legal = |level, strain| above(level, strain, x);
    // Responder's high card points to invite, and to bid game, by the
    // contract rules: with no bid left that any hand can invite with, which
    // is 2NT, game takes 25 opposite one more than opener promised.
    let can_invite = legal(2, None);
    let (invite, game) = if can_invite {
        (25 - x.hi.min(x.lo + 2), 25 - x.lo)
    } else {
        let game = 25 - x.hi.min(x.lo + 1);
        (game, game)
    };
    let weak = MAX.with_hcp(invite - 1);
    let inviting = (MIN.with_hcp(invite), MAX.with_hcp(game - 1));
    let game_values = MIN.with_hcp(game);

    // Suits in which responder knows of eight cards between them, with the
    // cards responder needs for that.
    let mut fits: Vec<(Suit, u8)> = Vec::new();
    if let Some(strain) = x.strain {
        if strain == response {
            fits.push((response, 4));
        } else {
            fits.push((strain, 8 - x.length));
        }
    }
    if is_major(opening) && x.strain != Some(opening) {
        fits.push((opening, 3));
    }

    // Game.
    for &(suit, need) in fits.iter().filter(|(suit, _)| is_major(*suit)) {
        if legal(4, Some(suit)) {
            s.add(
                "Game",
                &regex(&call(4, Some(suit))),
                "A fit",
                Forcing::Passable,
                [fit(game_values.min_len(suit, need), MAX)],
            );
        }
    }
    if legal(3, None) {
        s.add(
            "Game",
            &regex("3N"),
            "",
            Forcing::Passable,
            [fit(game_values, MAX)],
        );
    }
    // Sign-offs, which opener passes.
    let mut sign_offs: Vec<String> = Vec::new();
    // A new suit forces, except at the two level after a 1NT rebid.
    for q in suits_except(&[opening, response]) {
        if Some(q) == x.strain {
            continue;
        }
        let qlevel = (1..=3).find(|&l| legal(l, Some(q)));
        if let Some(qlevel) = qlevel {
            if x.strain.is_none() && x.level == 1 && qlevel == 2 {
                sign_offs.push(call(qlevel, Some(q)));
                s.add(
                    "New suit",
                    &regex(&call(qlevel, Some(q))),
                    "5+ cards, to play",
                    Forcing::Passable,
                    [fit(MIN.min_len(q, 5), weak)],
                );
            } else {
                s.add(
                    "New suit",
                    &regex(&call(qlevel, Some(q))),
                    "4+ cards",
                    Forcing::Forcing,
                    [fit(game_values.min_len(q, 4), MAX)],
                );
            }
        }
    }
    // An invitation.
    if invite < game {
        for &(suit, need) in fits.iter() {
            if legal(3, Some(suit)) {
                s.add(
                    "Game invite",
                    &regex(&call(3, Some(suit))),
                    "A fit",
                    Forcing::Passable,
                    [fit(inviting.0.min_len(suit, need), inviting.1)],
                );
            }
        }
        if legal(3, Some(response)) {
            s.add(
                "Game invite",
                &regex(&call(3, Some(response))),
                "6+ cards",
                Forcing::Passable,
                [fit(inviting.0.min_len(response, 6), inviting.1)],
            );
        }
        if legal(2, None) {
            s.add(
                "Game invite",
                &regex("2N"),
                "",
                Forcing::Passable,
                [fit(inviting.0, inviting.1)],
            );
        }
    }
    // A sign-off, which opener passes.
    if invite > 6 {
        if let Some(z) = x.strain {
            if x.level == 1 && is_major(z) && z != response {
                sign_offs.push(call(2, Some(z)));
                s.add(
                    "Raise",
                    &regex(&call(2, Some(z))),
                    "4+ card support",
                    Forcing::Passable,
                    [fit(MIN.min_len(z, 4), weak)],
                );
            }
        }
        if !x.forcing {
            if let Some((suit, need)) = fits.first() {
                if Some(*suit) == x.strain {
                    s.add(
                        "Pass",
                        &regex("P"),
                        "A fit, too weak to invite",
                        Forcing::Passable,
                        [fit(MIN.min_len(*suit, *need), weak)],
                    );
                }
            }
        }
        if legal(2, Some(response)) {
            sign_offs.push(call(2, Some(response)));
            s.add(
                "Rebid",
                &regex(&call(2, Some(response))),
                "6+ cards",
                Forcing::Passable,
                [fit(MIN.min_len(response, 6), weak)],
            );
        }
        if legal(2, Some(opening)) {
            sign_offs.push(call(2, Some(opening)));
            s.add(
                "Preference",
                &regex(&call(2, Some(opening))),
                "3+ cards",
                Forcing::Passable,
                [fit(MIN.min_len(opening, 3), weak)],
            );
        }
        if legal(1, None) {
            sign_offs.push("1N".to_string());
            s.add(
                "Rebid",
                &regex("1N"),
                "",
                Forcing::Passable,
                [fit(MIN, weak)],
            );
        }
        if !x.forcing {
            s.add(
                "Pass",
                &regex("P"),
                "Too weak to invite",
                Forcing::Passable,
                [fit(MIN, weak)],
            );
        }
    }
    // Opener may not be passed in a forcing rebid.
    if x.forcing {
        let nt = if legal(2, None) { 2 } else { 3 };
        s.add(
            "Rebid",
            &regex(&call(nt, None)),
            "Nothing else fits",
            Forcing::Passable,
            [fit(MIN, MAX)],
        );
    }

    for bid in sign_offs {
        s.add(
            "Pass",
            &format!("{after}{bid} P P$"),
            "Partner signed off",
            Forcing::Passable,
            [fit(MIN, MAX)],
        );
    }

    // Opener accepts an invitation with enough for game: game in a major
    // fit, and otherwise 3NT.
    let accept = MIN.with_hcp(25 - (game - 1));
    for &(suit, _) in fits.iter() {
        if legal(3, Some(suit)) {
            let game = if is_major(suit) {
                call(4, Some(suit))
            } else {
                "3N".to_string()
            };
            s.add(
                "Game",
                &format!("{after}{} P {game}$", call(3, Some(suit))),
                "Accepting",
                Forcing::Passable,
                [fit(accept, MAX)],
            );
        }
    }
    if legal(2, None) {
        s.add(
            "Game",
            &format!("{after}2N P 3N$"),
            "Accepting",
            Forcing::Passable,
            [fit(accept, MAX)],
        );
    }
    if legal(3, Some(response)) {
        let r = response.latin();
        if is_major(response) {
            s.add(
                "Game",
                &format!("{after}3{r} P 4{r}$"),
                "Accepting, 2+ card support",
                Forcing::Passable,
                [fit(accept.min_len(response, 2), MAX)],
            );
        }
        s.add(
            "Game",
            &format!("{after}3{r} P 3N$"),
            "Accepting",
            Forcing::Passable,
            [fit(accept, MAX)],
        );
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
