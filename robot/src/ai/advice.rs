//! Bidding advice: what the auction has shown so far and which bids suit a
//! hand, put into words from the same tables the robot bids by.
//!
//! Each bid means a box of bounds on a `HandValuation`, so the advice reads
//! every bound as a phrase such as "5+ spades" or "6–9 high card points".
//! When describing a bid it leaves out bounds that only shape a balanced hand
//! or break a tie with another bid, such as the "at most 5 hearts" of a 1S
//! opening.  The bid the robot would make is the fitting one whose table entry
//! comes first (`first_fit`), that
//! fits the hand, and the other bids are explained by the bounds the hand
//! misses.  Natural bids state what the partnership needs between them, so
//! for those the advice weighs the hand against what partner has shown, and
//! turns them into the contracts worth aiming for (see `contracts`).  The
//! advice is arranged by strain: for each, the contracts to aim for and then
//! the closest bids about it.  It reads the same as HTML or plain text (see
//! `text`).
use super::{legal_bids, Convention, Fit, Forcing, ShapeRule};
use crate::Bid;
use bridge_deck::{Cards, HandValuation, Suit};
#[cfg(test)]
use display_as::format_as;
use display_as::{with_template, DisplayAs, HTML, UTF8};
#[cfg(test)]
use expect_test::{expect, Expect};
use std::fmt::{self, Display, Formatter};

#[macro_use]
mod text;
mod contracts;

use text::{AddTo, List, Text};

/// Advice on the next bid of an auction.
pub(crate) struct Advice {
    /// What each player has shown, the hand, and whether it must bid.
    situation: Vec<Text>,
    /// The bid the robot would make, and why, unless no bid fits.
    suggestion: Option<(Bid, Text)>,
    /// For each strain worth mentioning, its heading, then the contracts in
    /// it to aim for and the other bids about it that come closest to fitting.
    strains: Vec<(Text, Vec<Line>)>,
    /// Every other bid the convention understands, with its strain's heading.
    every_bid: Vec<(Text, Line)>,
}

#[with_template("[%" "%]" "advice.html")]
impl DisplayAs<HTML> for Advice {}

#[with_template("[%" "%]" "advice.txt")]
impl DisplayAs<UTF8> for Advice {}

/// A bid, and what to say about it.
#[derive(Clone)]
struct Line {
    bid: Bid,
    text: Text,
}

/// How many other bids the advice lists by strain, so that it fits on a
/// screen.
const OTHERS: usize = 5;

impl Convention {
    /// Advice for the player holding `hand` on the next bid after `history`.
    pub(crate) fn advice(&self, history: &[Bid], hand: Cards) -> Advice {
        let hand = hand.values();
        let n = history.len();
        let players = [
            ("Partner", 2),
            ("Your right-hand opponent", 1),
            ("Your left-hand opponent", 3),
            ("You", 0),
        ];
        let partner = self.shown(history, 2);
        let mut situation: Vec<_> = players
            .iter()
            .filter_map(|&(who, offset)| self.what_was_shown(history, who, offset))
            .collect();
        situation.push(hand_summary(&hand));
        situation.extend(contracts::together(&hand, &partner));

        let mut bids = history.to_vec();
        bids.push(Bid::Pass);
        let candidates: Vec<Candidate> = legal_bids(history)
            .into_iter()
            .rev()
            .filter_map(|bid| {
                *bids.last_mut().unwrap() = bid;
                let meaning = self.refine(&bids)?;
                let rank = self.first_fit(&bids, &hand, usize::MAX).map(|(i, _)| i);
                Some(Candidate::new(
                    bid,
                    meaning,
                    self.entry(&bids),
                    &hand,
                    &partner,
                    rank,
                ))
            })
            .collect();
        // The robot bids the fitting bid whose table entry comes first, and
        // the highest of those when one entry describes several.
        let chosen = candidates.iter().filter(|c| c.fits).min_by_key(|c| c.rank);

        // Partner's bid forces us only if the opponent after them passed.
        let partner_forcing = if n >= 2 && history[n - 1] == Bid::Pass {
            self.check_forcing(&history[..n - 1])
        } else {
            Forcing::Passable
        };
        let pass = candidates.iter().find(|c| c.bid == Bid::Pass);
        situation.extend(match (partner_forcing, pass) {
            (Forcing::GameForcing, _) => Some(text!(
                "Partner's bid is game forcing, so keep bidding until you reach game."
            )),
            (Forcing::Forcing, _) => Some(text!("Partner's bid is forcing, so you must bid.")),
            (_, Some(pass)) if !pass.fits => Some(text!(
                "You should bid, since a pass would show ",
                pass.misses.clone(),
                "."
            )),
            _ => None,
        });

        let suggestion = chosen.map(|c| (c.bid, text!(c.title(), ".", c.reason(&hand))));

        let chosen_bid = chosen.map(|c| c.bid);
        let others = candidates.iter().filter(|c| Some(c.bid) != chosen_bid);
        let mut closest: Vec<&Candidate> = others
            .clone()
            // Natural bids are said as contracts, and why not to pass above.
            .filter(|c| c.needs.is_none() && (c.bid != Bid::Pass || c.fits))
            .collect();
        closest.sort_by_key(|c| (!c.fits, c.distance));
        closest.truncate(OTHERS);
        closest.sort_by_key(|c| c.bid.level());

        let contracts = self.contracts(
            history,
            &hand,
            &partner,
            &self.shown(history, 0),
            &candidates,
        );
        let strains = Strain::all()
            .filter_map(|strain| {
                let lines: Vec<Line> = contracts
                    .iter()
                    // The suggestion already says why to bid it.
                    .filter(|(s, line)| *s == strain && Some(line.bid) != chosen_bid)
                    .map(|(_, line)| line.clone())
                    .chain(
                        closest
                            .iter()
                            .filter(|c| c.strain == strain)
                            .map(|c| c.comparison(chosen)),
                    )
                    .collect();
                if lines.is_empty() {
                    None
                } else {
                    Some((strain.label(), lines))
                }
            })
            .collect();

        let mut every_bid: Vec<&Candidate> = others.collect();
        every_bid.sort_by_key(|c| (c.strain.order(), c.bid.level()));
        let every_bid = every_bid
            .into_iter()
            .map(|c| (c.strain.label(), c.comparison(chosen)))
            .collect();

        Advice {
            situation,
            suggestion,
            strains,
            every_bid,
        }
    }

    /// The bounds a bid states outright, if it is described only by entries
    /// that do: not by a natural bid, whose bounds say what the partnership
    /// needs between them.
    fn stated(&self) -> Option<(HandValuation, HandValuation)> {
        match self {
            Convention::Simple { .. } | Convention::Rule { .. } => Some(self.bounds()),
            Convention::Either(options) if options.iter().all(|c| c.stated().is_some()) => {
                Some(self.bounds())
            }
            _ => None,
        }
    }

    /// The table entry that gives the last bid of `bids` its meaning, before
    /// `refine` adjusts it for earlier bids.
    fn entry(&self, bids: &[Bid]) -> Option<&Convention> {
        match self {
            Convention::Ordered { conventions, .. } => conventions.iter().find(|c| c.applies(bids)),
            c => Some(c),
        }
    }

    /// The bids made by the player `offset` seats before the next bidder,
    /// with the bounds that their table entries state outright.
    fn shown(&self, history: &[Bid], offset: usize) -> Shown {
        let n = history.len();
        let mut shown = Shown {
            bids: Vec::new(),
            min: HandValuation::MIN,
            max: HandValuation::MAX,
        };
        for i in (0..n).filter(|i| (n - i) % 4 == offset) {
            shown.bids.push((i, history[i]));
            if let Some((min, max)) = self.refine(&history[..=i]).and_then(|c| c.stated()) {
                shown.min = shown.min.max(min);
                shown.max = shown.max.min(max);
            }
        }
        shown
    }

    /// A sentence on what `who` has shown with their bids, if they have bid
    /// anything but pass, or are partner.
    fn what_was_shown(&self, history: &[Bid], who: &str, offset: usize) -> Option<Text> {
        let shown = self.shown(history, offset);
        let first_bid = shown.bids.iter().position(|&(_, b)| b != Bid::Pass);
        if first_bid.is_none() && (offset != 2 || shown.bids.is_empty()) {
            return None;
        }
        let opening = history.iter().position(|&b| b != Bid::Pass);
        let said: Vec<Text> = shown
            .bids
            .iter()
            .enumerate()
            // A pass after a real bid says little.
            .filter(|&(j, &(_, b))| b != Bid::Pass || first_bid.map_or(true, |f| j < f))
            .map(|(_, &(i, bid))| match bid {
                Bid::Pass => text!("passed"),
                Bid::Double => text!("doubled"),
                Bid::Redouble => text!("redoubled"),
                _ if Some(i) == opening => text!("opened ", bid),
                _ => text!("bid ", bid),
            })
            .collect();
        let several = said.len() > 1;
        let features = describe(&shown.min, &shown.max);
        Some(if features.is_empty() {
            text!(who, " ", List::then(said), ".")
        } else if several {
            text!(
                who,
                " ",
                List::then(said),
                ", which together show ",
                List::and(features),
                "."
            )
        } else {
            text!(
                who,
                " ",
                List::then(said),
                ", showing ",
                List::and(features),
                "."
            )
        })
    }
}

/// What a player has shown with their bids, as bounds combined from the
/// table entries for each bid.  This is everything the advice knows about the
/// other hands, so it is the place for a better estimate, such as one drawn
/// from deals simulated to fit the whole auction.
#[derive(Clone)]
struct Shown {
    /// Their bids, with where each sits in the auction.
    bids: Vec<(usize, Bid)>,
    min: HandValuation,
    max: HandValuation,
}

impl Shown {
    /// The range partner has shown on `m`.  Points count long points too, as
    /// `adjust_for_earlier_bids` does.
    fn range(&self, m: Measure) -> (u8, u8) {
        match m {
            Measure::Hcp => (
                self.min.hcp.max(self.min.lhcp),
                self.max.hcp.min(self.max.lhcp),
            ),
            _ => (m.of(&self.min), m.of(&self.max)),
        }
    }
    /// The least they have shown, counting long points as `range` does.
    fn least(&self) -> HandValuation {
        HandValuation {
            hcp: self.range(Measure::Hcp).0,
            ..self.min
        }
    }
    /// What they will have shown after also making a bid that means `meaning`.
    fn and(&self, meaning: &Convention) -> Shown {
        let mut shown = self.clone();
        if let Some((min, max)) = meaning.stated() {
            shown.min = shown.min.max(min);
            shown.max = shown.max.min(max);
        }
        shown
    }
}

/// A bid the convention understands, measured against the hand.
struct Candidate {
    bid: Bid,
    /// What the bid means, adjusted for earlier bids.
    meaning: Convention,
    /// For a natural bid, what the partnership needs between them.
    needs: Option<Vec<Feature>>,
    /// For a natural bid, how what partner has shown bears on it.
    verdict: Option<Text>,
    fits: bool,
    /// For a bid that fits the hand, the position in the table of the entry
    /// that the robot uses, where a lower one is preferred.
    rank: Option<usize>,
    /// The bounds the hand misses, each with what the hand has.
    misses: Text,
    /// How far the hand is from fitting, to find the closest misses.
    distance: u32,
    strain: Strain,
}

impl Candidate {
    fn new(
        bid: Bid,
        meaning: Convention,
        entry: Option<&Convention>,
        hand: &HandValuation,
        partner: &Shown,
        rank: Option<usize>,
    ) -> Self {
        let (min, _) = meaning.bounds();
        let (misses, distance) = closest_misses(&meaning, hand);
        let together = match entry {
            Some(Convention::Natural { min, .. }) => Some(*min),
            _ => None,
        };
        Candidate {
            bid,
            needs: together.map(|t| describe(&t, &HandValuation::MAX)),
            verdict: together.map(|t| Reach::of(&t, hand, partner).verdict()),
            fits: rank.is_some(),
            rank,
            misses: text!(List::and(misses)),
            distance,
            // A natural bid's own bounds are reduced by what partner has
            // shown, so its entry says better what it is about.
            strain: Strain::of(bid, together.as_ref().unwrap_or(&min)),
            meaning,
        }
    }

    /// The bid's name, and its description if it has one.
    fn title(&self) -> Text {
        match &self.meaning {
            Convention::Simple {
                the_name,
                the_description,
                min,
                max,
                ..
            } => {
                let d = the_description.as_str();
                let said = describe(min, max).iter().any(|f| f.to_string() == d);
                if d.is_empty() || d == *the_name || said {
                    text!(*the_name)
                } else {
                    text!(*the_name, " (", Text::html(the_description), ")")
                }
            }
            Convention::Rule {
                the_name,
                the_description,
                ..
            } => {
                let d = the_description.as_str();
                if d.is_empty() || d == *the_name {
                    text!(*the_name)
                } else {
                    text!(*the_name, " (", Text::html(the_description), ")")
                }
            }
            Convention::Natural { the_name, .. } | Convention::Forced { the_name, .. } => {
                text!(*the_name)
            }
            Convention::Either(_) => Text::html(&self.meaning.name()),
            Convention::Ordered { .. } => text!(),
        }
    }

    /// Why the robot makes this bid, which fits the hand, as sentences that
    /// each start with a space.
    fn reason(&self, hand: &HandValuation) -> Text {
        if let Some(needs) = &self.needs {
            return text!(
                " It needs ",
                List::and(needs.clone()),
                " between you, and you have that."
            );
        }
        let (min, max) = self.meaning.bounds();
        let mut features = describe(&min, &max);
        for shape in fitting_shape(&self.meaning, hand) {
            // A balanced hand may be asked for by the bounds as well.
            if !features.iter().any(|f| f.to_string() == shape.to_string()) {
                features.push(shape);
            }
        }
        let shows = if features.is_empty() {
            text!()
        } else {
            text!(" It shows ", List::and(features), ".")
        };
        let place = place_in_range(&min, &max, hand).unwrap_or_default();
        let forcing = match self.meaning.is_forcing() {
            Forcing::Forcing => " Partner must bid again.",
            Forcing::GameForcing => " It commits you both to game.",
            _ => "",
        };
        text!(shows, place, forcing)
    }

    /// How this bid compares with the one the robot would make.
    fn comparison(&self, chosen: Option<&Candidate>) -> Line {
        let title = self.title();
        let text = match (chosen, &self.needs, &self.verdict) {
            (Some(chosen), ..) if self.fits => text!(
                title,
                ": also fits, but the system prefers ",
                chosen.bid,
                "."
            ),
            (None, ..) if self.fits => text!(title, ": fits."),
            (_, Some(needs), Some(verdict)) => text!(
                title,
                ": it needs ",
                List::and(needs.clone()),
                " between you; ",
                verdict.clone(),
                "."
            ),
            _ => text!(title, ": needs ", self.misses.clone(), "."),
        };
        Line {
            bid: self.bid,
            text,
        }
    }
}
/// What a bid is about, to group bids that are about the same thing.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Strain {
    Suit(Suit),
    Notrump,
    Double,
    /// An artificial bid that shows no suit, such as Stayman.
    Other,
    Pass,
}

impl Strain {
    /// What a bid with lower bounds `min` is about.  A suit bid that asks for
    /// a card or more of its suit is about that suit.  Otherwise a bid is
    /// about the longest suit it asks for, if that is three or more cards, so
    /// that a transfer counts as the suit it shows.  A suit bid that shows no
    /// suit, such as Stayman, is about something else.
    fn of(bid: Bid, min: &HandValuation) -> Self {
        let own = match bid {
            Bid::Pass => return Strain::Pass,
            Bid::Double | Bid::Redouble => return Strain::Double,
            Bid::Suit(_, suit) => Some(suit),
            Bid::NT(_) => None,
        };
        match own {
            Some(suit) if min.length[suit] > 0 => return Strain::Suit(suit),
            _ => {}
        }
        let longest = Suit::ALL
            .iter()
            .copied()
            .max_by_key(|&s| min.length[s])
            .unwrap();
        if min.length[longest] >= 3 {
            Strain::Suit(longest)
        } else if own.is_some() {
            Strain::Other
        } else {
            Strain::Notrump
        }
    }
    /// Every strain, spades first as on a convention card.
    fn all() -> impl Iterator<Item = Strain> {
        let suits = Suit::ALL.iter().rev().map(|&s| Strain::Suit(s));
        suits.chain(
            [Strain::Notrump, Strain::Double, Strain::Other, Strain::Pass]
                .iter()
                .copied(),
        )
    }
    fn order(self) -> usize {
        Strain::all().position(|s| s == self).unwrap()
    }
    /// The heading for bids about this strain.
    fn label(self) -> Text {
        match self {
            Strain::Suit(suit) => text!(suit),
            Strain::Notrump => text!("NT"),
            Strain::Double => text!("X"),
            Strain::Other => text!("Other"),
            Strain::Pass => text!("Pass"),
        }
    }
}

/// One of the bounds in a `HandValuation`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Measure {
    Length(Suit),
    Hcp,
    Lhcp,
    Shcp,
    Losers,
    HcpIn(Suit),
    HcpOutside(Suit),
}

impl Measure {
    /// Every measure, in the order the advice mentions them.
    fn all() -> impl Iterator<Item = Measure> {
        let suits = || Suit::ALL.iter().rev().copied();
        suits()
            .map(Measure::Length)
            .chain(
                [Measure::Hcp, Measure::Lhcp, Measure::Shcp, Measure::Losers]
                    .iter()
                    .copied(),
            )
            .chain(suits().map(Measure::HcpIn))
            .chain(suits().map(Measure::HcpOutside))
    }
    fn of(self, v: &HandValuation) -> u8 {
        match self {
            Measure::Length(suit) => v.length[suit],
            Measure::Hcp => v.hcp,
            Measure::Lhcp => v.lhcp,
            Measure::Shcp => v.shcp,
            Measure::Losers => v.nltc2,
            Measure::HcpIn(suit) => v.hcp_in_suit[suit],
            Measure::HcpOutside(suit) => v.hcp_outside_suit[suit],
        }
    }
    /// The value that means no upper bound.
    fn limit(self) -> u8 {
        self.of(&HandValuation::MAX)
    }
    /// The value that means one card, point or loser.
    fn one(self) -> u8 {
        match self {
            // The loser count is stored doubled.
            Measure::Losers => 2,
            _ => 1,
        }
    }
    /// Writes `amount` of this measure, as in "5+ spades", in the singular
    /// when `one`.
    fn write(self, f: &mut Formatter, amount: impl Display, one: bool) -> fmt::Result {
        let (singular, plural) = match self {
            Measure::Length(Suit::Clubs) => ("club", "clubs"),
            Measure::Length(Suit::Diamonds) => ("diamond", "diamonds"),
            Measure::Length(Suit::Hearts) => ("heart", "hearts"),
            Measure::Length(Suit::Spades) => ("spade", "spades"),
            Measure::Hcp | Measure::HcpIn(_) | Measure::HcpOutside(_) => {
                ("high card point", "high card points")
            }
            Measure::Lhcp => ("long high card point", "long high card points"),
            Measure::Shcp => ("short high card point", "short high card points"),
            Measure::Losers => ("loser", "losers"),
        };
        write!(f, "{} {}", amount, if one { singular } else { plural })?;
        match self {
            Measure::HcpIn(suit) => write!(f, " in {}", suit.name()),
            Measure::HcpOutside(suit) => write!(f, " outside {}", suit.name()),
            _ => Ok(()),
        }
    }
    /// Is this one of the ways of counting the strength of the whole hand?
    fn is_points(self) -> bool {
        matches!(self, Measure::Hcp | Measure::Lhcp | Measure::Shcp)
    }
    /// How much one card or point of this measure counts when looking for
    /// the bid that the hand comes closest to.
    fn weight(self) -> u32 {
        match self {
            Measure::Length(_) => 3,
            _ => 1,
        }
    }
}

/// A value of a measure as a number of cards, points or losers, as in "7"
/// or "6½".
#[derive(Clone, Copy)]
struct Amount(Measure, u8);

impl Display for Amount {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Amount(Measure::Losers, v) if v % 2 == 1 => write!(f, "{}½", v / 2),
            Amount(Measure::Losers, v) => write!(f, "{}", v / 2),
            Amount(_, v) => write!(f, "{}", v),
        }
    }
}

/// A value of a measure with its noun, as in "7 high card points".
#[derive(Clone, Copy)]
struct Count(Measure, u8);

impl Count {
    fn is_one(self) -> bool {
        self.1 == self.0.one()
    }
}

impl Display for Count {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0.write(f, Amount(self.0, self.1), self.is_one())
    }
}

/// The bounds on one measure, as in "5+ spades" or "6–9 high card points".
#[derive(Clone, Copy)]
struct Bound {
    m: Measure,
    min: u8,
    max: u8,
}

impl Bound {
    /// The bounds `min..=max` on `m`, unless they bound nothing.
    fn new(m: Measure, min: u8, max: u8) -> Option<Self> {
        if min == 0 && max >= m.limit() {
            None
        } else {
            Some(Bound { m, min, max })
        }
    }
    /// At least `min` of `m`.
    fn at_least(m: Measure, min: u8) -> Option<Self> {
        Bound::new(m, min, m.limit())
    }
    /// Just the amounts, as in "6–9" or "5+".
    fn range(self) -> Range {
        Range(self)
    }
}

impl Display for Bound {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let one = self.max < self.m.limit() && Count(self.m, self.max).is_one();
        self.m.write(f, self.range(), one)
    }
}

/// The amounts of a `Bound`, as in "6–9" or "5+".
#[derive(Clone, Copy)]
struct Range(Bound);

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Bound { m, min, max } = self.0;
        if max == 0 {
            write!(f, "no")
        } else if min == max {
            write!(f, "exactly {}", Amount(m, min))
        } else if max >= m.limit() {
            write!(f, "{}+", Amount(m, min))
        } else if min == 0 {
            write!(f, "at most {}", Amount(m, max))
        } else {
            write!(f, "{}–{}", Amount(m, min), Amount(m, max))
        }
    }
}

/// Something a bid's bounds ask for.
#[derive(Clone, Copy)]
enum Feature {
    Balanced,
    Bound(Bound),
    Shape(ShapeRule),
}

impl Display for Feature {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Feature::Balanced => write!(f, "a balanced hand"),
            Feature::Bound(b) => b.fmt(f),
            Feature::Shape(ShapeRule::AtLeastAsLong(a, b)) => {
                write!(f, "{} at least as long as {}", a.name(), b.name())
            }
            Feature::Shape(ShapeRule::Longer(a, b)) => {
                write!(f, "{} longer than {}", a.name(), b.name())
            }
            Feature::Shape(ShapeRule::Balanced) => write!(f, "a balanced hand"),
        }
    }
}

/// Something a hand lacks for a bid, with what it has.
struct Miss {
    feature: Feature,
    have: Option<Amount>,
}

impl Display for Miss {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.have {
            Some(have) => write!(f, "{} (you have {})", self.feature, have),
            None => self.feature.fmt(f),
        }
    }
}

add_as_words!(Count, Bound, Range, Feature, Miss, Need);

/// Do these bounds ask for a balanced hand?
fn is_balanced(min: &HandValuation, max: &HandValuation) -> bool {
    Suit::ALL
        .iter()
        .all(|&s| min.length[s] >= 2 && max.length[s] <= 5)
}

/// The bounds on suit lengths beyond those of a balanced hand.
fn beyond_balanced(min: u8, max: u8) -> (u8, u8) {
    (
        if min == 2 { 0 } else { min },
        if max == 5 { 13 } else { max },
    )
}

/// What the bounds `min..=max` ask for that is worth mentioning.
fn describe(min: &HandValuation, max: &HandValuation) -> Vec<Feature> {
    let balanced = is_balanced(min, max);
    let mut features = Vec::new();
    if balanced {
        features.push(Feature::Balanced);
    }
    let mut counted_points = false;
    for m in Measure::all() {
        let (mut lo, mut hi) = (m.of(min), m.of(max));
        match m {
            Measure::Length(_) if balanced => (lo, hi) = beyond_balanced(lo, hi),
            // A limit of five or more cards in a suit the bid doesn't ask
            // for only breaks a tie with another bid.
            Measure::Length(_) if lo == 0 && hi >= 5 => hi = m.limit(),
            // One way of counting points is enough to say how strong a hand is.
            _ if m.is_points() && counted_points => continue,
            _ => {}
        }
        if let Some(b) = Bound::new(m, lo, hi) {
            counted_points |= m.is_points();
            features.push(Feature::Bound(b));
        }
    }
    features
}

/// What the bounds `min..=max` ask for that `hand` misses, and how far it is
/// from fitting.
fn misses(min: &HandValuation, max: &HandValuation, hand: &HandValuation) -> (Vec<Miss>, u32) {
    let balanced = is_balanced(min, max);
    let mut misses = Vec::new();
    let mut distance = 0;
    if balanced && !is_balanced(hand, hand) {
        misses.push(Miss {
            feature: Feature::Balanced,
            have: None,
        });
        distance += 3;
    }
    let mut counted_points = false;
    for m in Measure::all() {
        let (mut lo, mut hi) = (m.of(min), m.of(max));
        if let (Measure::Length(_), true) = (m, balanced) {
            (lo, hi) = beyond_balanced(lo, hi);
        }
        let have = m.of(hand);
        if have < lo || have > hi {
            distance += m.weight() * (lo.saturating_sub(have) + have.saturating_sub(hi)) as u32;
            if m.is_points() && std::mem::replace(&mut counted_points, true) {
                continue;
            }
            misses.push(Miss {
                feature: Feature::Bound(Bound {
                    m,
                    min: lo,
                    max: hi,
                }),
                have: Some(Amount(m, have)),
            });
        }
    }
    (misses, distance)
}

/// The bounds and shape rules of `fit` that `hand` misses, and how far it is
/// from fitting.
fn fit_misses(fit: &Fit, hand: &HandValuation) -> (Vec<Miss>, u32) {
    let (mut misses, mut distance) = self::misses(&fit.min, &fit.max, hand);
    for &rule in fit.shape.iter().filter(|r| !r.holds(hand)) {
        let feature = Feature::Shape(rule);
        if !misses
            .iter()
            .any(|m| m.feature.to_string() == feature.to_string())
        {
            misses.push(Miss {
                feature,
                have: None,
            });
            distance += 3;
        }
    }
    (misses, distance)
}

/// What `hand` misses of the way of fitting `meaning` that it comes closest
/// to, and how far it is from fitting that way.
fn closest_misses(meaning: &Convention, hand: &HandValuation) -> (Vec<Miss>, u32) {
    match meaning {
        Convention::Rule { fits, .. } => fits
            .iter()
            .map(|f| fit_misses(f, hand))
            .min_by_key(|&(_, distance)| distance)
            .unwrap_or_default(),
        Convention::Either(options) => options
            .iter()
            .map(|c| closest_misses(c, hand))
            .min_by_key(|&(_, distance)| distance)
            .unwrap_or_default(),
        c => {
            let (min, max) = c.bounds();
            misses(&min, &max, hand)
        }
    }
}

/// The shape rules of the way that `hand` fits `meaning`.
fn fitting_shape(meaning: &Convention, hand: &HandValuation) -> Vec<Feature> {
    match meaning {
        Convention::Rule { fits, .. } => fits
            .iter()
            .find(|f| f.holds(hand))
            .map(|f| f.shape.iter().map(|&r| Feature::Shape(r)).collect())
            .unwrap_or_default(),
        Convention::Either(options) => options
            .iter()
            .find(|c| c.fits(hand))
            .map(|c| fitting_shape(c, hand))
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

/// How what a partnership needs compares with a hand plus what partner has
/// shown.
enum Reach {
    /// The hand and the least partner has shown are enough.
    Sure,
    /// Enough if partner has these.
    IfPartnerHas(Vec<Need>),
    /// More than partner can have, given the most they have shown.
    OutOfReach(Vec<Bound>),
}

impl Reach {
    /// How `together`, what the partnership needs, compares with `hand` plus
    /// what `partner` has shown.
    fn of(together: &HandValuation, hand: &HandValuation, partner: &Shown) -> Self {
        let mut out_of_reach = Vec::new();
        let mut partner_needs = Vec::new();
        for m in Measure::all() {
            let need = m.of(together).saturating_sub(m.of(hand));
            let (lo, hi) = partner.range(m);
            if need > hi {
                out_of_reach.extend(Bound::new(m, 0, hi));
            } else if need > lo {
                partner_needs.push(Need {
                    m,
                    amount: need,
                    shown: (lo, hi),
                });
            }
        }
        if !out_of_reach.is_empty() {
            Reach::OutOfReach(out_of_reach)
        } else if !partner_needs.is_empty() {
            Reach::IfPartnerHas(partner_needs)
        } else {
            Reach::Sure
        }
    }
    fn verdict(&self) -> Text {
        match self {
            Reach::Sure => text!("you have that between you"),
            Reach::IfPartnerHas(needs) => {
                let needs = needs
                    .iter()
                    .map(|&n| match n.shown() {
                        Some(shown) => text!(n, " (they have shown ", shown, ")"),
                        None => text!(n),
                    })
                    .collect();
                text!("that works if partner has ", List::and(needs))
            }
            Reach::OutOfReach(shown) => text!(
                "partner has shown ",
                List::and(shown.clone()),
                ", so that is out of reach"
            ),
        }
    }
}

/// Something partner needs to hold for a contract, as in "8+ high card
/// points".
#[derive(Clone, Copy)]
struct Need {
    m: Measure,
    /// How much of `m` partner needs.
    amount: u8,
    /// The range of `m` that partner has shown.
    shown: (u8, u8),
}

impl Need {
    /// The range partner has shown, if they have shown any.
    fn shown(self) -> Option<Range> {
        Bound::new(self.m, self.shown.0, self.shown.1).map(Bound::range)
    }
    /// How far this asks beyond the least partner has shown, to compare the
    /// bids that might lead to a contract.
    fn stretch(&self) -> u32 {
        self.m.weight() * (self.amount - self.shown.0) as u32
    }
}

impl Display for Need {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match Bound::at_least(self.m, self.amount) {
            Some(b) => b.fmt(f),
            None => Ok(()),
        }
    }
}

/// Where the hand sits in the range of the first point count the bounds
/// limit on both sides, when it is near one end.
fn place_in_range(min: &HandValuation, max: &HandValuation, hand: &HandValuation) -> Option<Text> {
    let m = [Measure::Hcp, Measure::Lhcp, Measure::Shcp]
        .iter()
        .copied()
        .find(|m| m.of(min) > 0 && m.of(max) < m.limit())?;
    let (lo, hi, have) = (m.of(min), m.of(max), Count(m, m.of(hand)));
    let third = hi.saturating_sub(lo) / 3;
    let end = if have.1 <= lo + third {
        "bottom"
    } else if have.1 >= hi - third {
        "top"
    } else {
        return None;
    };
    let verb = if have.is_one() { "is" } else { "are" };
    Some(text!(
        " Your ",
        have,
        " ",
        verb,
        " near the ",
        end,
        " of that range."
    ))
}

/// The hand's point counts and shape.
fn hand_summary(hand: &HandValuation) -> Text {
    use Suit::*;
    let l = hand.length;
    text!(
        "Your hand: ",
        hand.hcp,
        " HCP, ",
        hand.lhcp,
        " LHCP and ",
        hand.shcp,
        " SHCP, with ",
        l[Spades],
        Spades,
        " ",
        l[Hearts],
        Hearts,
        " ",
        l[Diamonds],
        Diamonds,
        " ",
        l[Clubs],
        Clubs,
        "."
    )
}

/// Checks the SAYC advice for `hand` after `bids`, as plain text, up to the
/// list of every bid, which starts closed.
#[cfg(test)]
fn check(bids: &[Bid], hand: &str, expect: Expect) {
    use std::str::FromStr;
    let advice = Convention::sayc().advice(bids, Cards::from_str(hand).unwrap());
    let text = format_as!(UTF8, advice).into_string();
    expect.assert_eq(text.split("Every bid:").next().unwrap().trim_end());
}

#[test]
fn advice_after_a_one_spade_opening() {
    use crate::Bid::*;
    use bridge_deck::Suit::Spades;
    let one_spade = [Suit(1, Spades), Pass];
    // Toward 4♠, 2♠ tells partner about the spade fit, which 1NT would not.
    check(
        &one_spade,
        "S Q32 H KJ4 D J543 C 654",
        expect![[r#"
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 7 HCP, 7 LHCP and 7 SHCP, with 3♠ 3♥ 4♦ 3♣.
            Together you have 20–28 high card points and 8+ spades.
            You should bid, since a pass would show at most 5 high card points (you have 7).
            Suggested: 2♠ Single raise (3+ card support). It shows 3+ spades and 6–9 high card points. Your 7 high card points are near the bottom of that range.
            ♠ 3♠ part-score: needs 8+ spades and 24+ high card points between you; that works if partner has 17+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 18+ high card points.
            ♠ 4♠ game: needs 8+ spades and 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 20+ high card points.
            ♠ 2NT Jacoby 2NT (4+ card support): needs 4+ spades (you have 3) and 13+ high card points (you have 7).
            ♠ 3♠ Limit raise (3+ card support): needs 10–11 high card points (you have 7).
            ♦ 2♦ Two-level response (New suit): needs 10+ high card points (you have 7).
            NT 3NT game: needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21). After 2♠, partner knows you have 6+ high card points, so they can bid it with 20+ high card points.
            NT 1NT Response (No fit and no suit to bid): also fits, but the system prefers 2♠.
            Other 2♣ Two-level response: needs 10+ high card points (you have 7)."#]],
    );
    check(
        &one_spade,
        "S 32 H 5432 D 432 C 5432",
        expect![[r#"
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 0 HCP, 0 LHCP and 1 SHCP, with 2♠ 4♥ 3♦ 4♣.
            Together you have 13–21 high card points.
            Suggested: Pass Response pass. It shows at most 5 high card points.
            ♠ 2♠ Single raise (3+ card support): needs 3+ spades (you have 2) and 6–9 high card points (you have 0).
            ♠ 3♠ Limit raise (3+ card support): needs 3+ spades (you have 2) and 10–11 high card points (you have 0).
            ♥ 2♥ Two-level response (New suit): needs 5+ hearts (you have 4) and 10+ high card points (you have 0).
            NT 1NT Response (No fit and no suit to bid): needs 6–9 high card points (you have 0).
            Other 2♣ Two-level response: needs 10+ high card points (you have 0)."#]],
    );
}

#[test]
fn advice_after_one_notrump() {
    use crate::Bid::*;
    // A transfer is listed with the suit it shows.
    check(
        &[NT(1), Pass],
        "S KJ32 H Q432 D 32 C 432",
        expect![[r#"
            Partner opened 1NT, showing a balanced hand and 15–17 high card points.
            Your hand: 6 HCP, 6 LHCP and 7 SHCP, with 4♠ 4♥ 2♦ 3♣.
            Together you have 21–23 high card points.
            Suggested: Pass Response pass (Too weak to invite). It shows at most 7 high card points.
            ♠ 2♥ Jacoby transfer (5+ spades): needs 5+ spades (you have 4).
            ♥ 2♦ Jacoby transfer (5+ hearts): needs 5+ hearts (you have 4).
            NT 2NT Game invite: needs 8–9 high card points (you have 6).
            NT 3NT Game (To play): needs 10–15 high card points (you have 6).
            Other 2♣ Stayman (A 4 card major): needs 8+ high card points (you have 6)."#]],
    );
}

#[test]
fn advice_follows_every_seeded_auction() {
    for system in super::BiddingSystem::ALL {
        let convention = system.convention();
        for (_, hands, bids) in super::seeded_deals(system.convention(), 100) {
            for i in 0..bids.len() {
                let text =
                    format_as!(HTML, convention.advice(&bids[..i], hands[i % 4])).into_string();
                // The suggestion is the bid the robot made.
                let made =
                    format_as!(HTML, "Suggested: <strong>" bids[i] "</strong>").into_string();
                assert!(
                    text.contains(&made)
                        || (bids[i] == Bid::Pass && text.contains("robot would pass")),
                    "{} after {}:\n{}",
                    made,
                    super::bids_string(&bids[..i]),
                    text
                );
            }
        }
    }
}

#[test]
fn advice_on_a_rebid_after_one_notrump() {
    use crate::Bid::*;
    use bridge_deck::Suit::Hearts;
    // 2NT shows more than the opening did, so partner can bid 3NT.
    check(
        &[Suit(1, Hearts), Pass, NT(1), Pass],
        "S A2 H KQ976 D AJ3 C KQ2",
        expect![[r#"
            Partner bid 1NT, showing 6–9 high card points.
            You opened 1♥, showing 5+ hearts and 13–21 long high card points.
            Your hand: 19 HCP, 20 LHCP and 20 SHCP, with 2♠ 5♥ 3♦ 3♣.
            Together you have 25–28 high card points.
            Suggested: 2NT Rebid (Balanced, 18-19 hcp). It shows a balanced hand and 18–19 high card points. Your 19 high card points are near the top of that range.
            ♠ 2♠ New suit (Reverse): needs 4+ spades (you have 2).
            ♥ 3♥ part-score: needs 8+ hearts and 24+ high card points between you; that works if partner has 3+ hearts. Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts.
            ♥ 4♥ game: needs 8+ hearts and 26+ high card points between you; that works if partner has 3+ hearts and 7+ high card points (they have shown 6–9). Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts and 8+ high card points.
            ♥ 2♥ Rebid (6+ card suit): needs 6+ hearts (you have 5) and 13–15 long high card points (you have 20).
            ♥ 3♥ Jump rebid (6+ card suit, invitational): needs 6+ hearts (you have 5) and 16–18 long high card points (you have 20).
            ♦ 2♦ New suit: needs 4+ diamonds (you have 3).
            ♣ 2♣ New suit: needs 4+ clubs (you have 3).
            NT 3NT game: needs 26+ high card points between you; that works if partner has 7+ high card points (they have shown 6–9). Bidding 2NT shows more than you have so far: partner then knows you have 18+ high card points, so they can bid it with 8+ high card points."#]],
    );
}

#[test]
fn advice_in_competition() {
    use crate::Bid::*;
    use bridge_deck::Suit::*;
    // With nine trumps the three level is safe.
    check(
        &[Suit(1, Hearts), Suit(1, Spades)],
        "S 32 H K432 D A432 C 432",
        expect![[r#"
            Partner opened 1♥, showing 5+ hearts and 13–21 long high card points.
            Your right-hand opponent bid 1♠, showing 5+ spades and 8–16 high card points.
            Your hand: 7 HCP, 7 LHCP and 8 SHCP, with 2♠ 4♥ 4♦ 3♣.
            Together you have 20–28 high card points and 9+ hearts.
            Suggested: 3♥ Law of total tricks. It needs 9+ hearts between you, and you have that.
            NT 3NT game: needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21)."#]],
    );
}
