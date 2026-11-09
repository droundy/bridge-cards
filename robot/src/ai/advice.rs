//! Bidding advice: what the auction has shown so far and which bids suit a
//! hand, put into words from the same tables the robot bids by.
//!
//! Each bid means a box of bounds on a `HandValuation`, so the advice reads
//! every bound as a phrase such as "5+ spades" or "6–9 high card points".
//! When describing a bid it leaves out bounds that only shape a balanced hand
//! or break a tie with another bid, such as the "at most 5 hearts" of a 1S
//! opening.  The bid the robot would make is the fitting one whose table entry
//! comes first (`first_fit`), and the other bids are explained by the bounds
//! the hand misses.  Natural bids state what the partnership needs between them, so
//! for those the advice weighs the hand against what partner has shown, and
//! turns them into the contracts worth aiming for (see `contracts`).  The
//! advice is arranged by strain: for each, the contracts to aim for and then
//! the closest bids about it.  The sentences are built with `wording`, so
//! counts, lists and verbs agree wherever a phrase ends up, and the advice
//! reads the same as HTML or plain text.
use super::rules::Rule;
use super::{bids_string, legal_bids, Convention, Fit, Forcing, ShapeRule};
use crate::Bid;
use bridge_deck::{Cards, HandValuation, Suit};
#[cfg(test)]
use display_as::format_as;
use display_as::{with_template, DisplayAs, Format, FormattedString, LaTeX, HTML, UTF8};
#[cfg(test)]
use expect_test::{expect, Expect};
use std::fmt::{self, Formatter};
use wording::{
    and, at_least, between, count, numbered, then, Amount, Form, Markup, Number, Text, Word,
};

mod contracts;

const HCP: Word = Word::noun("high card point", "high card points");
const LHCP: Word = Word::noun("long high card point", "long high card points");
const SHCP: Word = Word::noun("short high card point", "short high card points");
const LOSER: Word = Word::noun("loser", "losers");
const KNOW: Word = Word::verb("knows", "know");

fn partner_text() -> Text {
    numbered("partner", Number::Singular)
}

fn you() -> Text {
    numbered("you", Number::Plural)
}

fn they() -> Text {
    numbered("they", Number::Plural)
}

/// HTML that a table entry wrote out, which plain text and LaTeX show as the
/// text in it.
struct Raw(FormattedString<HTML>);

/// HTML as plain text: without its tags, and with its escapes undone.
fn plain(html: &str) -> String {
    let mut text = String::new();
    let mut in_tag = false;
    for c in html.chars() {
        match c {
            '<' => in_tag = true,
            '>' => in_tag = false,
            c if !in_tag => text.push(c),
            _ => {}
        }
    }
    [
        ("&lt;", "<"),
        ("&gt;", ">"),
        ("&quot;", "\""),
        ("&#x27;", "'"),
        ("&#x2f;", "/"),
        ("&amp;", "&"),
    ]
    .iter()
    .fold(text, |text, (escaped, c)| text.replace(escaped, c))
}

impl Markup for Raw {
    fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.0.as_str())
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(&plain(self.0.as_str()))
    }
    fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        LaTeX::escape(f, &plain(self.0.as_str()))
    }
}

/// How many cards a hand has in a suit, as in "3♠", with nothing between the
/// number and the suit.
struct Length(u8, Suit);

impl Markup for Length {
    fn html(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        Markup::html(&self.1, form, f)
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1.unicode())
    }
    fn latex(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        Markup::latex(&self.1, form, f)
    }
}

/// A suit's symbol, as a heading, which is not a noun and so is never spelled
/// out in plain text.
struct Symbol(Suit);

impl Markup for Symbol {
    fn html(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        Markup::html(&self.0, form, f)
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0.unicode())
    }
    fn latex(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        Markup::latex(&self.0, form, f)
    }
}

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
        // With nothing else, it bids the contract the rules know.
        let known = self.contract_known(history, &hand).map(|(bid, _)| bid);
        let chosen = candidates
            .iter()
            .filter(|c| c.fits())
            .min_by_key(|c| c.rank)
            .or_else(|| candidates.iter().find(|c| Some(c.bid) == known));

        // Partner's bid forces us only if the opponent after them passed.
        let partner_forcing = if n >= 2 && history[n - 1] == Bid::Pass {
            self.check_forcing(&history[..n - 1])
        } else {
            Forcing::Passable
        };
        let pass = candidates.iter().find(|c| c.bid == Bid::Pass);
        situation.extend(match (partner_forcing, pass) {
            (Forcing::GameForcing, _) => Some(Text::from(
                "Partner's bid is game forcing, so keep bidding until you reach game.",
            )),
            (Forcing::Forcing, _) => Some(Text::from("Partner's bid is forcing, so you must bid.")),
            (_, Some(pass)) if !pass.fits() => Some(
                Text::from("You should bid, since a pass would show") + pass.misses.clone() + ".",
            ),
            _ => None,
        });

        let suggestion = chosen.map(|c| (c.bid, c.title() + "." + c.reason(&hand)));

        let chosen_bid = chosen.map(|c| c.bid);
        let others = candidates.iter().filter(|c| Some(c.bid) != chosen_bid);
        let mut closest: Vec<&Candidate> = others
            .clone()
            // Natural bids, and the contracts the rules place, are said as
            // contracts, and why not to pass above.
            .filter(|c| c.needs.is_none() && !c.places() && (c.bid != Bid::Pass || c.fits()))
            .collect();
        closest.sort_by_key(|c| (!c.fits(), c.distance));
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
            Convention::Ordered { conventions, .. } => {
                let auction = bids_string(bids);
                self.candidates(&auction)
                    .into_iter()
                    .find(|&i| match &conventions[i] {
                        Convention::Simple { regex, .. } | Convention::Rule { regex, .. } => {
                            regex.is_match(&auction)
                        }
                        _ => false,
                    })
                    .or_else(|| self.default_for(&auction, bids))
                    .map(|i| &conventions[i])
            }
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
                Bid::Pass => Text::from("passed"),
                Bid::Double => Text::from("doubled"),
                Bid::Redouble => Text::from("redoubled"),
                _ if Some(i) == opening => Text::from("opened") + bid,
                _ => Text::from("bid") + bid,
            })
            .collect();
        let several = said.len() > 1;
        let features = describe(&shown.min, &shown.max);
        let said = Text::from(who) + then(said);
        Some(
            if features.is_empty() {
                said
            } else if several {
                said + ", which together show" + phrases(features)
            } else {
                said + ", showing" + phrases(features)
            }
            .sentence(),
        )
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
    /// The range partner has shown on `m`.  Points count whichever kind they
    /// promised, long or dummy points too, as the contract rules do.
    fn range(&self, m: Measure) -> (u8, u8) {
        match m {
            Measure::Hcp => (
                self.min.hcp.max(self.min.lhcp).max(self.min.shcp),
                self.max.hcp.min(self.max.lhcp).min(self.max.shcp),
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
    needs: Option<Text>,
    /// For a natural bid, how what partner has shown bears on it.
    verdict: Option<Text>,
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
            needs: together.map(|t| {
                Text::from("it needs") + phrases(describe(&t, &HandValuation::MAX)) + "between you"
            }),
            verdict: together.map(|t| Reach::of(&t, hand, partner).verdict()),
            rank,
            misses: and(misses.into_iter().map(Miss::text)),
            distance,
            // A natural bid's own bounds are reduced by what partner has
            // shown, so its entry says better what it is about.
            strain: Strain::of(bid, together.as_ref().unwrap_or(&min)),
            meaning,
        }
    }

    /// Does the contract rules' meaning of this bid place a contract, which
    /// the advice says as a contract rather than as a bid?
    fn places(&self) -> bool {
        matches!(&self.meaning, Convention::Contracted { reason, .. } if reason.rule != Rule::Invite)
    }

    /// Does the hand fit this bid?
    fn fits(&self) -> bool {
        self.rank.is_some()
    }

    /// The bid's name, and its description if it has one.
    fn title(&self) -> Text {
        match &self.meaning {
            Convention::Simple {
                the_name,
                the_description,
                ..
            }
            | Convention::Rule {
                the_name,
                the_description,
                ..
            } => {
                let d = the_description.as_str();
                let name = Text::from(*the_name);
                if d.is_empty() || d == *the_name {
                    name
                } else {
                    name + " (" + Raw(the_description.clone()) + ")"
                }
            }
            Convention::Contracted {
                the_name, reason, ..
            } if reason.rule == Rule::Invite => {
                Text::from(*the_name) + " (invites" + reason.contract + ")"
            }
            c => Text::from(c.the_name()),
        }
    }

    /// Why the robot makes this bid, which fits the hand.
    fn reason(&self, hand: &HandValuation) -> Text {
        if let Some(needs) = &self.needs {
            return needs.clone().capitalized() + ", and you have that.";
        }
        if let Convention::Contracted { reason, .. } = &self.meaning {
            return reason.text();
        }
        let (min, max) = self.meaning.bounds();
        let mut features = describe(&min, &max);
        features.extend(fitting_shape(&self.meaning, hand));
        let mut reason = Text::new();
        if !features.is_empty() {
            reason += Text::from("It shows") + phrases(features) + ".";
        }
        reason += place_in_range(&min, &max, hand);
        match self.meaning.is_forcing() {
            Forcing::Forcing => reason += "Partner must bid again.",
            Forcing::GameForcing => reason += "It commits you both to game.",
            _ => {}
        }
        reason
    }

    /// How this bid compares with the one the robot would make.
    fn comparison(&self, chosen: Option<&Candidate>) -> Line {
        let title = self.title();
        let text = match (chosen, &self.needs, &self.verdict) {
            (Some(chosen), ..) if self.fits() => {
                title + ": also fits, but the system prefers" + chosen.bid + "."
            }
            (None, ..) if self.fits() => title + ": fits.",
            (_, Some(needs), Some(verdict)) => {
                title + ":" + needs.clone() + ";" + verdict.clone() + "."
            }
            _ => title + ": needs" + self.misses.clone() + ".",
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
    /// Where this strain comes in `all`.
    fn order(self) -> usize {
        match self {
            Strain::Suit(suit) => 3 - suit as usize,
            Strain::Notrump => 4,
            Strain::Double => 5,
            Strain::Other => 6,
            Strain::Pass => 7,
        }
    }
    /// The heading for bids about this strain.
    fn label(self) -> Text {
        match self {
            Strain::Suit(suit) => Symbol(suit).into(),
            Strain::Notrump => "NT".into(),
            Strain::Double => "X".into(),
            Strain::Other => "Other".into(),
            Strain::Pass => "Pass".into(),
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
    /// A value of this measure as an `Amount` for a quantity.
    fn quantity(self, value: u8) -> Amount {
        match self {
            // The loser count is stored doubled, so it is a number of halves.
            Measure::Losers => Amount::halves(value.into()),
            _ => Amount::from(value),
        }
    }
    /// A value of this measure as a number of cards, points or losers, as in
    /// "7" or "6½".
    fn amount(self, value: u8) -> String {
        self.quantity(value).to_string()
    }
    /// What this measure counts, as in "high card points in spades".
    fn noun(self) -> Text {
        match self {
            Measure::Length(suit) => suit.into(),
            Measure::Hcp => HCP.into(),
            Measure::Lhcp => LHCP.into(),
            Measure::Shcp => SHCP.into(),
            Measure::Losers => LOSER.into(),
            Measure::HcpIn(suit) => Text::from(HCP) + "in" + suit,
            Measure::HcpOutside(suit) => Text::from(HCP) + "outside" + suit,
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
    /// The bounds in words, as in "5+ spades" or "at most 1 loser".
    fn text(self) -> Text {
        let Bound { m, min, max } = self;
        if max >= m.limit() && min != max {
            at_least(m.quantity(min), m.noun())
        } else {
            between(m.quantity(min), m.quantity(max), m.noun())
        }
    }
    /// Just the amounts, as in "6–9" or "5+".
    fn amounts(self) -> String {
        let Bound { m, min, max } = self;
        if max == 0 {
            "no".to_string()
        } else if min == max {
            format!("exactly {}", m.amount(min))
        } else if max >= m.limit() {
            format!("{}+", m.amount(min))
        } else if min == 0 {
            format!("at most {}", m.amount(max))
        } else {
            format!("{}–{}", m.amount(min), m.amount(max))
        }
    }
}

/// Something a bid's bounds ask for.
#[derive(Clone, Copy)]
enum Feature {
    Bound(Bound),
    Shape(ShapeRule),
}

impl Feature {
    fn text(self) -> Text {
        match self {
            Feature::Shape(ShapeRule::Balanced) => Text::from("a balanced hand"),
            Feature::Bound(b) => b.text(),
            Feature::Shape(ShapeRule::AtLeastAsLong(a, b)) => {
                Text::from(a) + "at least as long as" + b
            }
            Feature::Shape(ShapeRule::Longer(a, b)) => Text::from(a) + "longer than" + b,
        }
    }
}

/// The features as a list, as in "a balanced hand and 18–19 high card points".
fn phrases(features: Vec<Feature>) -> Text {
    and(features.into_iter().map(Feature::text))
}

/// Something a hand lacks for a bid, with what it has.
struct Miss {
    feature: Feature,
    /// The measure and amount the hand has, if that is worth saying.
    have: Option<(Measure, u8)>,
}

impl Miss {
    fn text(self) -> Text {
        match self.have {
            Some((m, have)) => {
                self.feature.text() + " (" + (you() + Word::HAS + m.amount(have)) + ")"
            }
            None => self.feature.text(),
        }
    }
}

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
        features.push(Feature::Shape(ShapeRule::Balanced));
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
    if balanced && !ShapeRule::Balanced.holds(hand) {
        misses.push(Miss {
            feature: Feature::Shape(ShapeRule::Balanced),
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
                have: Some((m, have)),
            });
        }
    }
    (misses, distance)
}

/// The bounds and shape rules of `fit` that `hand` misses, and how far it is
/// from fitting.
fn fit_misses(fit: &Fit, hand: &HandValuation) -> (Vec<Miss>, u32) {
    let (mut misses, mut distance) = self::misses(&fit.min, &fit.max, hand);
    // `misses` already asks for a balanced hand when the bounds do.
    let balanced = is_balanced(&fit.min, &fit.max);
    for &rule in fit
        .shape
        .iter()
        .filter(|&&r| !r.holds(hand) && !(balanced && r == ShapeRule::Balanced))
    {
        misses.push(Miss {
            feature: Feature::Shape(rule),
            have: None,
        });
        distance += 3;
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
        // What the robot bids it with, even when it shows nothing.
        Convention::Contracted { min, max, .. } => misses(min, max, hand),
        c => {
            let (min, max) = c.bounds();
            misses(&min, &max, hand)
        }
    }
}

/// The shape rules of the way that `hand` fits `meaning`.
fn fitting_rules(meaning: &Convention, hand: &HandValuation) -> Vec<ShapeRule> {
    match meaning {
        Convention::Rule { fits, .. } => fits
            .iter()
            .find(|f| f.holds(hand))
            .map(|f| f.shape.clone())
            .unwrap_or_default(),
        Convention::Either(options) => options
            .iter()
            .find(|c| c.fits(hand))
            .map(|c| fitting_rules(c, hand))
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

/// The features for the shape rules of the way that `hand` fits `meaning`,
/// other than a balanced hand when `describe` says so already.
fn fitting_shape(meaning: &Convention, hand: &HandValuation) -> Vec<Feature> {
    let (min, max) = meaning.bounds();
    let balanced = is_balanced(&min, &max);
    fitting_rules(meaning, hand)
        .into_iter()
        .filter(|&r| !(balanced && r == ShapeRule::Balanced))
        .map(Feature::Shape)
        .collect()
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
            Reach::Sure => you() + Word::HAS + "that between you",
            Reach::IfPartnerHas(needs) => {
                let needs = needs.iter().map(|&n| match n.shown() {
                    Some(shown) => n.text() + " (" + (they() + Word::HAS + "shown" + shown) + ")",
                    None => n.text(),
                });
                Text::from("that works if") + (partner_text() + Word::HAS + and(needs))
            }
            Reach::OutOfReach(bounds) => {
                partner_text()
                    + Word::HAS
                    + "shown"
                    + and(bounds.iter().map(|b| b.text()))
                    + ", so that is out of reach"
            }
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
    /// The range partner has shown, if they have shown any, as amounts.
    fn shown(self) -> Option<String> {
        Bound::new(self.m, self.shown.0, self.shown.1).map(Bound::amounts)
    }
    /// How far this asks beyond the least partner has shown, to compare the
    /// bids that might lead to a contract.
    fn stretch(&self) -> u32 {
        self.m.weight() * (self.amount - self.shown.0) as u32
    }
    /// What partner needs, as in "8+ high card points".
    fn text(self) -> Text {
        Bound::at_least(self.m, self.amount).map_or_else(Text::new, Bound::text)
    }
}

/// Where the hand sits in the range of the first point count the bounds
/// limit on both sides, when it is near one end.
fn place_in_range(min: &HandValuation, max: &HandValuation, hand: &HandValuation) -> Option<Text> {
    let m = [Measure::Hcp, Measure::Lhcp, Measure::Shcp]
        .iter()
        .copied()
        .find(|m| m.of(min) > 0 && m.of(max) < m.limit())?;
    let (lo, hi, have) = (m.of(min), m.of(max), m.of(hand));
    let third = hi.saturating_sub(lo) / 3;
    let end = if have <= lo + third {
        "bottom"
    } else if have >= hi - third {
        "top"
    } else {
        return None;
    };
    Some(
        "Your" + count(m.quantity(have), m.noun()) + Word::IS + "near the" + end + "of that range.",
    )
}

/// The hand's point counts and shape.
fn hand_summary(hand: &HandValuation) -> Text {
    let s = |suit: Suit| Text::from(Length(hand.length[suit], suit));
    use Suit::*;
    Text::from("Your hand:")
        + hand.hcp
        + "HCP,"
        + hand.lhcp
        + "LHCP and"
        + hand.shcp
        + "SHCP, with"
        + s(Spades)
        + s(Hearts)
        + s(Diamonds)
        + s(Clubs)
        + "."
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
            Suggested: 2♠ Single raise (3+ card support). It shows 3+ spades and 6–9 short high card points. Your 7 short high card points are near the bottom of that range.
            ♠ 3♠ part-score: needs 8+ spades and 24+ high card points between you; that works if partner has 17+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 18+ high card points.
            ♠ 4♠ game: needs 8+ spades and 25+ high card points between you; that works if partner has 18+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 19+ high card points.
            ♠ 2NT Jacoby 2NT (4+ card support): needs 4+ spades (you have 3) and 13+ short high card points (you have 7).
            ♠ 3♠ Limit raise (invites 4♠): needs 10–11 short high card points (you have 7).
            ♦ 2♦ Two-level response (New suit): needs 10+ high card points (you have 7).
            NT 3NT game: needs 25+ high card points between you; that works if partner has 18+ high card points (they have shown 13–21). After 2♠, partner knows you have 6+ high card points, so they can bid it with 19+ high card points.
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
            ♠ 2♠ Single raise (3+ card support): needs 3+ spades (you have 2) and 6–9 short high card points (you have 1).
            ♠ 3♠ Limit raise (invites 4♠): needs 3+ spades (you have 2) and 10–11 short high card points (you have 1).
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
            ♠ 3♠ Game invite (invites 4♠): needs 6+ spades (you have 4) and 8–9 long high card points (you have 6).
            ♥ 2♦ Jacoby transfer (5+ hearts): needs 5+ hearts (you have 4).
            NT 2NT Game invite (invites 3NT): needs 8–9 high card points (you have 6).
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
            Partner bid 1NT, showing at most 3 spades and 6–9 high card points.
            You opened 1♥, showing 5+ hearts and 13–21 long high card points.
            Your hand: 19 HCP, 20 LHCP and 20 SHCP, with 2♠ 5♥ 3♦ 3♣.
            Together you have 25–28 high card points.
            Suggested: 2NT Rebid (Balanced, 18-19 hcp). It shows a balanced hand and 18–19 high card points. Your 19 high card points are near the top of that range.
            ♠ 2♠ New suit (Reverse): needs 4+ spades (you have 2).
            ♥ 3♥ part-score: needs 8+ hearts and 24+ high card points between you; that works if partner has 3+ hearts. Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts.
            ♥ 4♥ game: needs 8+ hearts and 25+ high card points between you; that works if partner has 3+ hearts. Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts and 7+ high card points.
            ♥ 3♥ Jump rebid (6+ card suit, invitational): needs 6+ hearts (you have 5) and 16–18 long high card points (you have 20).
            ♦ 2♦ New suit: needs 4+ diamonds (you have 3).
            ♣ 2♣ New suit: needs 4+ clubs (you have 3).
            NT 5NT game: needs 25+ high card points between you; you have that between you.
            NT 4NT Slam invite (invites 6NT): needs 25–26 high card points (you have 19)."#]],
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

#[test]
fn a_hand_that_is_not_balanced_misses_a_balanced_opening() {
    use std::str::FromStr;
    // Two doubletons are not balanced, though no suit is short or long.
    let advice =
        Convention::sayc().advice(&[], Cards::from_str("S AKJ32 H KQ32 D A2 C 32").unwrap());
    let text = format_as!(UTF8, advice).into_string();
    assert!(
        text.contains("1NT Opening 1NT: needs a balanced hand."),
        "{}",
        text
    );
}

#[test]
fn advice_html_is_well_formed() {
    // Every element the template opens is closed again.
    for system in super::BiddingSystem::ALL {
        let convention = system.convention();
        for (_, hands, bids) in super::seeded_deals(system.convention(), 20) {
            for i in 0..bids.len() {
                let html =
                    format_as!(HTML, convention.advice(&bids[..i], hands[i % 4])).into_string();
                for tag in ["details", "table", "tr", "th", "td", "p", "strong"] {
                    let opened = html.matches(&format!("<{} ", tag)).count()
                        + html.matches(&format!("<{}>", tag)).count();
                    let closed = html.matches(&format!("</{}>", tag)).count();
                    assert_eq!(
                        opened,
                        closed,
                        "<{}> after {}:\n{}",
                        tag,
                        super::bids_string(&bids[..i]),
                        html
                    );
                }
                assert_eq!(html.matches("id=\"advice\"").count(), 1);
                assert!(html.matches("id=\"every-bid\"").count() <= 1);
            }
        }
    }
}
