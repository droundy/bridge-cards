//! Bidding advice: what the auction has shown so far and which bids suit a
//! hand, put into words from the same tables the robot bids by.
//!
//! Each bid means a box of bounds on a `HandValuation`, so the advice reads
//! every bound as a phrase such as "5+ spades" or "6–9 high card points".
//! When describing a bid it leaves out bounds that only shape a balanced hand
//! or break a tie with another bid, such as the "at most 5 hearts" of a 1S
//! opening.  The bid the robot would make is the fitting one whose table entry
//! comes first (`first_fit`), and the other bids are explained by the bounds
//! the hand misses.  Natural bids state what the partnership needs between
//! them, so for those the advice weighs the hand against what partner has
//! shown.
//!
//! The sentences are built with `wording`, so that counts, lists and verbs
//! agree wherever a phrase ends up, and the advice can be shown as HTML, plain
//! text or LaTeX.
use super::{legal_bids, Convention, Fit, Forcing, ShapeRule};
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

/// Advice on the next bid of an auction.
pub(crate) struct Advice {
    /// What each player has shown, the hand, and whether it must bid.
    situation: Vec<Text>,
    /// The bid the robot would make, and why.
    suggestion: Text,
    /// The other bids closest to fitting, each with the suit it is about.
    others: Vec<(Text, Text)>,
}

#[with_template("[%" "%]" "advice.html")]
impl DisplayAs<HTML> for Advice {}

/// How many other bids the advice lists, so that it fits on a screen.
const OTHERS: usize = 6;

const HCP: Word = Word::noun("high card point", "high card points");
const LHCP: Word = Word::noun("long high card point", "long high card points");
const SHCP: Word = Word::noun("short high card point", "short high card points");
const LOSER: Word = Word::noun("loser", "losers");

fn partner() -> Text {
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

/// `html` as plain text: without its tags, and with its escapes undone.
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

/// A phrase in bold.
struct Strong(Text);

impl Markup for Strong {
    fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str("<strong>")?;
        <Text as DisplayAs<HTML>>::fmt(&self.0, f)?;
        f.write_str("</strong>")
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        <Text as DisplayAs<UTF8>>::fmt(&self.0, f)
    }
    fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(r"\textbf{")?;
        <Text as DisplayAs<LaTeX>>::fmt(&self.0, f)?;
        f.write_str("}")
    }
}

/// How many cards a hand has in a suit, as in "3♠", with nothing between the
/// number and the suit.
struct Length(u8, Suit);

impl Markup for Length {
    fn html(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        self.1.html(form, f)
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1.unicode())
    }
    fn latex(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)?;
        self.1.latex(form, f)
    }
}

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
        let chosen = candidates
            .iter()
            .filter(|c| c.rank.is_some())
            .min_by_key(|c| c.rank);

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

        let suggestion = match chosen {
            Some(c) => {
                Text::from("Suggested:") + Strong(c.bid.into()) + c.title() + "." + c.reason(&hand)
            }
            None => Text::from("No bid fits your hand, so the robot would pass."),
        };

        let mut others: Vec<&Candidate> = candidates
            .iter()
            .filter(|c| Some(c.bid) != chosen.map(|c| c.bid))
            // Why not to pass is said above.
            .filter(|c| c.bid != Bid::Pass || c.fits())
            .collect();
        others.sort_by_key(|c| (!c.fits(), c.distance));
        others.truncate(OTHERS);
        others.sort_by_key(|c| (c.strain.order(), c.bid.level()));
        let others = others
            .into_iter()
            .map(|c| (c.strain.label(), c.comparison(chosen)))
            .collect();

        Advice {
            situation,
            suggestion,
            others,
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
        let verbs: Vec<Text> = shown
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
        let several = verbs.len() > 1;
        let said = Text::from(who) + then(verbs);
        let words = describe(&shown.min, &shown.max);
        Some(
            if words.is_empty() {
                said
            } else if !several {
                said + ", showing" + and(words)
            } else {
                said + ", which together show" + and(words)
            }
            .sentence(),
        )
    }
}

/// What a player has shown with their bids.
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
                Text::from("it needs") + and(describe(&t, &HandValuation::MAX)) + "between you"
            }),
            verdict: together.map(|t| verdict(&t, hand, partner)),
            rank,
            misses: and(misses),
            distance,
            strain: Strain::of(bid, &min),
            meaning,
        }
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
            } => {
                let d = the_description.as_str();
                let name = Text::from(*the_name);
                if d.is_empty() || d == *the_name {
                    name
                } else {
                    name + " (" + Raw(the_description.clone()) + ")"
                }
            }
            c => Text::from(c.the_name()),
        }
    }

    /// Why the robot makes this bid, which fits the hand.
    fn reason(&self, hand: &HandValuation) -> Text {
        if let Some(needs) = &self.needs {
            return needs.clone().capitalized() + ", and you have that.";
        }
        let (min, max) = self.meaning.bounds();
        let mut words = describe(&min, &max);
        words.extend(fitting_shape(&self.meaning, hand));
        let mut reason = Text::new();
        if !words.is_empty() {
            reason += Text::from("It shows") + and(words) + ".";
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
    fn comparison(&self, chosen: Option<&Candidate>) -> Text {
        let title = Text::from(Strong(self.bid.into())) + self.title();
        if self.fits() {
            return match chosen {
                Some(chosen) => title + ": also fits, but the system prefers" + chosen.bid + ".",
                None => title + ": fits.",
            };
        }
        match (&self.needs, &self.verdict) {
            (Some(needs), Some(verdict)) => {
                title + ":" + needs.clone() + ";" + verdict.clone() + "."
            }
            _ => title + ": needs" + self.misses.clone() + ".",
        }
    }
}

/// What a bid is about, to group bids that are about the same thing.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Strain {
    Suit(Suit),
    Notrump,
    Double,
    Pass,
}

impl Strain {
    /// The suit a bid shows: the longest one its bounds ask for, if that is
    /// three or more cards, so that a transfer counts as the suit it shows.
    fn of(bid: Bid, min: &HandValuation) -> Self {
        let own = match bid {
            Bid::Pass => return Strain::Pass,
            Bid::Double | Bid::Redouble => return Strain::Double,
            Bid::Suit(_, suit) => Some(suit),
            Bid::NT(_) => None,
        };
        let longest = Suit::ALL
            .iter()
            .copied()
            .max_by_key(|&s| (min.length[s], Some(s) == own))
            .unwrap();
        if min.length[longest] >= 3 {
            Strain::Suit(longest)
        } else {
            own.map_or(Strain::Notrump, Strain::Suit)
        }
    }
    /// Spades first, as on a convention card.
    fn order(self) -> u8 {
        match self {
            Strain::Suit(suit) => 3 - suit as u8,
            Strain::Notrump => 4,
            Strain::Double => 5,
            Strain::Pass => 6,
        }
    }
    /// The heading for bids about this strain.
    fn label(self) -> Text {
        match self {
            Strain::Suit(suit) => suit.into(),
            Strain::Notrump => "NT".into(),
            Strain::Double => "X".into(),
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
    /// A value of this measure as a number of cards, points or losers.
    fn amount(self, value: u8) -> String {
        self.quantity(value).to_string()
    }
    /// A value of this measure as an `Amount` for a quantity.
    fn quantity(self, value: u8) -> Amount {
        match self {
            // The loser count is stored doubled, so it is a number of halves.
            Measure::Losers => Amount::halves(value.into()),
            _ => Amount::from(value),
        }
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

/// The words for the bounds `min..=max` on `m`, as in "5+ spades" or "at most
/// 1 loser", unless they bound nothing.
fn bound(m: Measure, min: u8, max: u8) -> Option<Text> {
    if min == 0 && max >= m.limit() {
        None
    } else if max >= m.limit() {
        Some(at_least(m.quantity(min), m.noun()))
    } else {
        Some(between(m.quantity(min), m.quantity(max), m.noun()))
    }
}

/// The bounds `min..=max` on `m` as amounts, as in "6–9" or "5+".
fn range(m: Measure, min: u8, max: u8) -> Option<String> {
    Some(if min == 0 && max >= m.limit() {
        return None;
    } else if max == 0 {
        "no".to_string()
    } else if min == max {
        format!("exactly {}", m.amount(min))
    } else if max >= m.limit() {
        format!("{}+", m.amount(min))
    } else if min == 0 {
        format!("at most {}", m.amount(max))
    } else {
        format!("{}–{}", m.amount(min), m.amount(max))
    })
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

/// The words for each bound in `min..=max` worth mentioning.
fn describe(min: &HandValuation, max: &HandValuation) -> Vec<Text> {
    let balanced = is_balanced(min, max);
    let mut words = Vec::new();
    if balanced {
        words.push(Text::from("a balanced hand"));
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
        if let Some(w) = bound(m, lo, hi) {
            counted_points |= m.is_points();
            words.push(w);
        }
    }
    words
}

/// The words for each bound in `min..=max` that `hand` misses, with what the
/// hand has, and how far it is from fitting.
fn misses(min: &HandValuation, max: &HandValuation, hand: &HandValuation) -> (Vec<Text>, u32) {
    let balanced = is_balanced(min, max);
    let mut words = Vec::new();
    let mut distance = 0;
    if balanced && !is_balanced(hand, hand) {
        words.push(Text::from("a balanced hand"));
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
            words.push(
                bound(m, lo, hi).unwrap() + " (" + (you() + Word::HAS + m.amount(have)) + ")",
            );
        }
    }
    (words, distance)
}

/// The words for a shape rule, such as "spades at least as long as hearts".
fn shape_words(rule: ShapeRule) -> Text {
    match rule {
        ShapeRule::AtLeastAsLong(a, b) => Text::from(a) + "at least as long as" + b,
        ShapeRule::Longer(a, b) => Text::from(a) + "longer than" + b,
        ShapeRule::Balanced => Text::from("a balanced hand"),
    }
}

/// The bounds and shape rules of `fit` that `hand` misses, and how far it is
/// from fitting.
fn fit_misses(fit: &Fit, hand: &HandValuation) -> (Vec<Text>, u32) {
    let (mut words, mut distance) = misses(&fit.min, &fit.max, hand);
    // `misses` already asks for a balanced hand when the bounds do.
    let balanced = is_balanced(&fit.min, &fit.max);
    for &rule in fit
        .shape
        .iter()
        .filter(|&&r| !r.holds(hand) && !(balanced && r == ShapeRule::Balanced))
    {
        words.push(shape_words(rule));
        distance += 3;
    }
    (words, distance)
}

/// What `hand` misses of the way of fitting `meaning` that it comes closest
/// to, and how far it is from fitting that way.
fn closest_misses(meaning: &Convention, hand: &HandValuation) -> (Vec<Text>, u32) {
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

/// The words for the shape rules of the way that `hand` fits `meaning`, other
/// than a balanced hand when `describe` says so already.
fn fitting_shape(meaning: &Convention, hand: &HandValuation) -> Vec<Text> {
    let (min, max) = meaning.bounds();
    let balanced = is_balanced(&min, &max);
    fitting_rules(meaning, hand)
        .into_iter()
        .filter(|&r| !(balanced && r == ShapeRule::Balanced))
        .map(shape_words)
        .collect()
}

/// How a natural bid's needs, `together`, compare with the hand plus what
/// partner has shown.
fn verdict(together: &HandValuation, hand: &HandValuation, partner_shown: &Shown) -> Text {
    let mut out_of_reach = Vec::new();
    let mut partner_needs = Vec::new();
    for m in Measure::all() {
        let need = m.of(together).saturating_sub(m.of(hand));
        let (lo, hi) = partner_shown.range(m);
        if need > hi {
            out_of_reach.push(bound(m, 0, hi).unwrap_or_default());
        } else if need > lo {
            let needs = bound(m, need, m.limit()).unwrap_or_default();
            partner_needs.push(match range(m, lo, hi) {
                Some(shown) => needs + " (" + (they() + Word::HAS + "shown" + shown) + ")",
                None => needs,
            });
        }
    }
    if !out_of_reach.is_empty() {
        partner() + Word::HAS + "shown" + and(out_of_reach) + ", so that is out of reach"
    } else if !partner_needs.is_empty() {
        Text::from("that works if") + (partner() + Word::HAS + and(partner_needs))
    } else {
        you() + Word::HAS + "that between you"
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

/// Checks the SAYC advice for `hand` after `bids`, as plain text with one
/// line for each paragraph or table row.
#[cfg(test)]
fn check(bids: &[Bid], hand: &str, expect: Expect) {
    use std::str::FromStr;
    let advice = Convention::sayc().advice(bids, Cards::from_str(hand).unwrap());
    let html = format_as!(HTML, advice).into_string().replace("</th>", " ");
    let text = plain(&html);
    let lines: Vec<&str> = text
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .collect();
    expect.assert_eq(&lines.join("\n"));
}

#[test]
fn advice_after_a_one_spade_opening() {
    use crate::Bid::*;
    use bridge_deck::Suit::Spades;
    let one_spade = [Suit(1, Spades), Pass];
    check(
        &one_spade,
        "S Q32 H KJ4 D J543 C 654",
        expect![[r#"
            Partner opened 1♠, showing 5+ ♠ and 13–21 long high card points.
            Your hand: 7 HCP, 7 LHCP and 7 SHCP, with 3♠ 3♥ 4♦ 3♣.
            You should bid, since a pass would show at most 5 high card points (you have 7).
            Suggested: 2♠ Single raise. It shows 3+ ♠ and 6–9 high card points. Your 7 high card points are near the bottom of that range.
            ♠ 2NT Jacoby 2NT: needs 4+ ♠ (you have 3) and 13+ high card points (you have 7).
            ♠ 3♠ Limit raise: needs 10–11 high card points (you have 7).
            ♠ 4♠ Natural game: it needs 8+ ♠ and 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21).
            ♦ 2♦ Two-level response: needs 10+ high card points (you have 7).
            ♣ 2♣ Two-level response: needs 10+ high card points (you have 7).
            NT 1NT Response: also fits, but the system prefers 2♠."#]],
    );
    check(
        &one_spade,
        "S 32 H 5432 D 432 C 5432",
        expect![[r#"
            Partner opened 1♠, showing 5+ ♠ and 13–21 long high card points.
            Your hand: 0 HCP, 0 LHCP and 1 SHCP, with 2♠ 4♥ 3♦ 4♣.
            Suggested: Pass Response pass. It shows at most 5 high card points.
            ♠ 2♠ Single raise: needs 3+ ♠ (you have 2) and 6–9 high card points (you have 0).
            ♠ 3♠ Limit raise: needs 3+ ♠ (you have 2) and 10–11 high card points (you have 0).
            ♥ 2♥ Two-level response: needs 5+ ♥ (you have 4) and 10+ high card points (you have 0).
            ♣ 2♣ Two-level response: needs 10+ high card points (you have 0).
            NT 1NT Response: needs 6–9 high card points (you have 0).
            NT 3NT Response: needs 15–17 high card points (you have 0)."#]],
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
            Suggested: Pass Response pass. It shows at most 7 high card points.
            ♠ 2♥ Jacoby transfer: needs 5+ ♠ (you have 4).
            ♠ 3♠ Natural: it needs 8+ ♠ and 24+ high card points between you; partner has shown at most 17 high card points, so that is out of reach.
            ♥ 2♦ Jacoby transfer: needs 5+ ♥ (you have 4).
            ♣ 2♣ Stayman: needs 8+ high card points (you have 6).
            NT 2NT Game invite: needs 8–9 high card points (you have 6).
            NT 3NT Game: needs 10–15 high card points (you have 6)."#]],
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
fn raw_markup_is_plain_outside_html() {
    let said = "Michael's <cue> & 2/1";
    let name = Text::from(Raw(format_as!(HTML, said)));
    assert_eq!(name.to_string(), said);
}

#[test]
fn a_balanced_hand_is_said_once() {
    use crate::Bid::*;
    use bridge_deck::Suit::Hearts;
    use std::str::FromStr;
    let advice = Convention::sayc().advice(
        &[Suit(1, Hearts), Pass, NT(1), Pass],
        Cards::from_str("S A2 H KQ976 D AJ3 C KQ2").unwrap(),
    );
    let suggestion = advice.suggestion.to_string();
    assert_eq!(
        suggestion.matches("balanced hand").count(),
        1,
        "{suggestion}"
    );
}
