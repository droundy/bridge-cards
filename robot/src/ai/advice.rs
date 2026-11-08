//! Bidding advice: what the auction has shown so far and which bids suit a
//! hand, put into words from the same tables the robot bids by.
//!
//! Each bid means a box of bounds on a `HandValuation`, so the advice reads
//! every bound as a phrase such as "5+ spades" or "6–9 high card points".
//! When describing a bid it leaves out bounds that only shape a balanced hand
//! or break a tie with another bid, such as the "at most 5 hearts" of a 1S
//! opening.  The bid the robot would make is the fitting one whose table entry
//! comes first (`first_fit`), and the other bids are explained by the bounds the hand
//! misses.  Natural bids state what the partnership needs between them, so
//! for those the advice weighs the hand against what partner has shown.
use super::{legal_bids, Convention, Fit, Forcing, ShapeRule};
use crate::Bid;
use bridge_deck::{Cards, HandValuation, Suit};
use display_as::{format_as, with_template, DisplayAs, FormattedString, HTML};
#[cfg(test)]
use expect_test::{expect, Expect};

/// Advice on the next bid of an auction.
pub(crate) struct Advice {
    /// What each player has shown, the hand, and whether it must bid.
    situation: Vec<FormattedString<HTML>>,
    /// The bid the robot would make, and why.
    suggestion: FormattedString<HTML>,
    /// The other bids closest to fitting, each with the suit it is about.
    others: Vec<(FormattedString<HTML>, FormattedString<HTML>)>,
}

#[with_template("[%" "%]" "advice.html")]
impl DisplayAs<HTML> for Advice {}

/// How many other bids the advice lists, so that it fits on a screen.
const OTHERS: usize = 6;

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
            (Forcing::GameForcing, _) => Some(format_as!(
                HTML,
                "Partner's bid is game forcing, so keep bidding until you reach game."
            )),
            (Forcing::Forcing, _) => Some(format_as!(
                HTML,
                "Partner's bid is forcing, so you must bid."
            )),
            (_, Some(pass)) if !pass.fits() => Some(format_as!(
                HTML,
                "You should bid, since a pass would show " pass.misses "."
            )),
            _ => None,
        });

        let suggestion = match chosen {
            Some(c) => format_as!(HTML, "Suggested: <strong>" c.bid "</strong> " c.title() ". "
                                  c.reason(&hand)),
            None => format_as!(HTML, "No bid fits your hand, so the robot would pass."),
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
    fn what_was_shown(
        &self,
        history: &[Bid],
        who: &str,
        offset: usize,
    ) -> Option<FormattedString<HTML>> {
        let shown = self.shown(history, offset);
        let first_bid = shown.bids.iter().position(|&(_, b)| b != Bid::Pass);
        if first_bid.is_none() && (offset != 2 || shown.bids.is_empty()) {
            return None;
        }
        let opening = history.iter().position(|&b| b != Bid::Pass);
        let verbs: Vec<_> = shown
            .bids
            .iter()
            .enumerate()
            // A pass after a real bid says little.
            .filter(|&(j, &(_, b))| b != Bid::Pass || first_bid.map_or(true, |f| j < f))
            .map(|(_, &(i, bid))| match bid {
                Bid::Pass => format_as!(HTML, "passed"),
                Bid::Double => format_as!(HTML, "doubled"),
                Bid::Redouble => format_as!(HTML, "redoubled"),
                _ if Some(i) == opening => format_as!(HTML, "opened " bid),
                _ => format_as!(HTML, "bid " bid),
            })
            .collect();
        let said = FormattedString::<HTML>::from_formatted(
            verbs
                .iter()
                .map(|v| v.as_str())
                .collect::<Vec<_>>()
                .join(", then "),
        );
        let words = describe(&shown.min, &shown.max);
        Some(if words.is_empty() {
            format_as!(HTML, who " " said ".")
        } else if verbs.len() == 1 {
            format_as!(HTML, who " " said ", showing " join(words) ".")
        } else {
            format_as!(HTML, who " " said ", which together show " join(words) ".")
        })
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
    needs: Option<String>,
    /// For a natural bid, how what partner has shown bears on it.
    verdict: Option<String>,
    /// For a bid that fits the hand, the position in the table of the entry
    /// that the robot uses, where a lower one is preferred.
    rank: Option<usize>,
    /// The bounds the hand misses, each with what the hand has.
    misses: String,
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
        let (min, max) = meaning.bounds();
        let (misses, distance) = closest_misses(&meaning, hand);
        let together = match entry {
            Some(Convention::Natural { min, .. }) => Some(*min),
            _ => None,
        };
        Candidate {
            bid,
            needs: together.map(|t| {
                let words = describe(&t, &HandValuation::MAX);
                format!("it needs {} between you", join(words))
            }),
            verdict: together.map(|t| verdict(&t, hand, partner)),
            rank,
            misses: join(misses),
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
    fn title(&self) -> FormattedString<HTML> {
        match &self.meaning {
            Convention::Simple {
                the_name,
                the_description,
                min,
                max,
                ..
            } => {
                let d = the_description.as_str();
                if d.is_empty() || d == *the_name || describe(min, max).iter().any(|w| w == d) {
                    format_as!(HTML, the_name)
                } else {
                    format_as!(HTML, the_name " (" the_description ")")
                }
            }
            c => c.name(),
        }
    }

    /// Why the robot makes this bid, which fits the hand.
    fn reason(&self, hand: &HandValuation) -> FormattedString<HTML> {
        if let Some(needs) = &self.needs {
            return format_as!(HTML, capitalize(needs) ", and you have that.");
        }
        let (min, max) = self.meaning.bounds();
        let mut words = describe(&min, &max);
        words.extend(fitting_shape(&self.meaning, hand));
        let shows = if words.is_empty() {
            String::new()
        } else {
            format!("It shows {}.", join(words))
        };
        let place = place_in_range(&min, &max, hand).unwrap_or_default();
        let forcing = match self.meaning.is_forcing() {
            Forcing::Forcing => " Partner must bid again.",
            Forcing::GameForcing => " It commits you both to game.",
            _ => "",
        };
        format_as!(HTML, shows "" place "" forcing)
    }

    /// How this bid compares with the one the robot would make.
    fn comparison(&self, chosen: Option<&Candidate>) -> FormattedString<HTML> {
        let title = format_as!(HTML, "<strong>" self.bid "</strong> " self.title());
        if self.fits() {
            return match chosen {
                Some(chosen) => format_as!(HTML, title ": also fits, but the system prefers "
                                           chosen.bid "."),
                None => format_as!(HTML, title ": fits."),
            };
        }
        match (&self.needs, &self.verdict) {
            (Some(needs), Some(verdict)) => format_as!(HTML, title ": " needs "; " verdict "."),
            _ => format_as!(HTML, title ": needs " self.misses "."),
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
    fn label(self) -> FormattedString<HTML> {
        match self {
            Strain::Suit(suit) => format_as!(HTML, suit),
            Strain::Notrump => format_as!(HTML, "NT"),
            Strain::Double => format_as!(HTML, "X"),
            Strain::Pass => format_as!(HTML, "Pass"),
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
        match self {
            // The loser count is stored doubled.
            Measure::Losers if value % 2 == 1 => format!("{}½", value / 2),
            Measure::Losers => format!("{}", value / 2),
            _ => format!("{}", value),
        }
    }
    /// The words for `amount` of this measure, as in "5+ spades".
    fn words(self, amount: &str, one: bool) -> String {
        let noun = |plural: &'static str| {
            if one {
                plural.trim_end_matches('s')
            } else {
                plural
            }
        };
        match self {
            Measure::Length(suit) => format!("{} {}", amount, noun(suit.name())),
            Measure::Hcp => format!("{} {}", amount, noun("high card points")),
            Measure::Lhcp => format!("{} {}", amount, noun("long high card points")),
            Measure::Shcp => format!("{} {}", amount, noun("short high card points")),
            Measure::Losers => format!("{} {}", amount, noun("losers")),
            Measure::HcpIn(suit) => {
                format!("{} {} in {}", amount, noun("high card points"), suit.name())
            }
            Measure::HcpOutside(suit) => {
                format!(
                    "{} {} outside {}",
                    amount,
                    noun("high card points"),
                    suit.name()
                )
            }
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

/// The words for the bounds `min..=max` on `m`, unless they bound nothing.
fn bound(m: Measure, min: u8, max: u8) -> Option<String> {
    let amount = range(m, min, max)?;
    Some(m.words(&amount, max < m.limit() && m.amount(max) == "1"))
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
fn describe(min: &HandValuation, max: &HandValuation) -> Vec<String> {
    let balanced = is_balanced(min, max);
    let mut words = Vec::new();
    if balanced {
        words.push("a balanced hand".to_string());
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
fn misses(min: &HandValuation, max: &HandValuation, hand: &HandValuation) -> (Vec<String>, u32) {
    let balanced = is_balanced(min, max);
    let mut words = Vec::new();
    let mut distance = 0;
    if balanced && !is_balanced(hand, hand) {
        words.push("a balanced hand".to_string());
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
            words.push(format!(
                "{} (you have {})",
                bound(m, lo, hi).unwrap(),
                m.amount(have)
            ));
        }
    }
    (words, distance)
}

/// The words for a shape rule, such as "spades at least as long as hearts".
fn shape_words(rule: ShapeRule) -> String {
    match rule {
        ShapeRule::AtLeastAsLong(a, b) => format!("{} at least as long as {}", a.name(), b.name()),
        ShapeRule::Longer(a, b) => format!("{} longer than {}", a.name(), b.name()),
        ShapeRule::Balanced => "a balanced hand".to_string(),
    }
}

/// The bounds and shape rules of `fit` that `hand` misses, and how far it is
/// from fitting.
fn fit_misses(fit: &Fit, hand: &HandValuation) -> (Vec<String>, u32) {
    let (mut words, mut distance) = misses(&fit.min, &fit.max, hand);
    for &rule in fit.shape.iter().filter(|r| !r.holds(hand)) {
        let w = shape_words(rule);
        if !words.contains(&w) {
            words.push(w);
            distance += 3;
        }
    }
    (words, distance)
}

/// What `hand` misses of the way of fitting `meaning` that it comes closest
/// to, and how far it is from fitting that way.
fn closest_misses(meaning: &Convention, hand: &HandValuation) -> (Vec<String>, u32) {
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

/// The words for the shape rules of the way that `hand` fits `meaning`.
fn fitting_shape(meaning: &Convention, hand: &HandValuation) -> Vec<String> {
    match meaning {
        Convention::Rule { fits, .. } => fits
            .iter()
            .find(|f| f.holds(hand))
            .map(|f| f.shape.iter().map(|&r| shape_words(r)).collect())
            .unwrap_or_default(),
        Convention::Either(options) => options
            .iter()
            .find(|c| c.fits(hand))
            .map(|c| fitting_shape(c, hand))
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

/// How a natural bid's needs, `together`, compare with the hand plus what
/// partner has shown.
fn verdict(together: &HandValuation, hand: &HandValuation, partner: &Shown) -> String {
    let mut out_of_reach = Vec::new();
    let mut partner_needs = Vec::new();
    for m in Measure::all() {
        let need = m.of(together).saturating_sub(m.of(hand));
        let (lo, hi) = partner.range(m);
        if need > hi {
            out_of_reach.push(bound(m, 0, hi).unwrap_or_default());
        } else if need > lo {
            let needs = bound(m, need, m.limit()).unwrap_or_default();
            partner_needs.push(match range(m, lo, hi) {
                Some(shown) => format!("{} (they have shown {})", needs, shown),
                None => needs,
            });
        }
    }
    if !out_of_reach.is_empty() {
        format!(
            "partner has shown {}, so that is out of reach",
            join(out_of_reach)
        )
    } else if !partner_needs.is_empty() {
        format!("that works if partner has {}", join(partner_needs))
    } else {
        "you have that between you".to_string()
    }
}

/// Where the hand sits in the range of the first point count the bounds
/// limit on both sides, when it is near one end.
fn place_in_range(
    min: &HandValuation,
    max: &HandValuation,
    hand: &HandValuation,
) -> Option<String> {
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
    Some(format!(
        " Your {} are near the {} of that range.",
        m.words(&m.amount(have), have == 1),
        end
    ))
}

/// The hand's point counts and shape.
fn hand_summary(hand: &HandValuation) -> FormattedString<HTML> {
    let s = |suit: Suit| format_as!(HTML, hand.length[suit] "" suit);
    use Suit::*;
    format_as!(HTML, "Your hand: " hand.hcp " HCP, " hand.lhcp " LHCP and " hand.shcp " SHCP, with "
        s(Spades) " " s(Hearts) " " s(Diamonds) " " s(Clubs) ".")
}

/// "a", "a and b", "a, b and c".
fn join(mut words: Vec<String>) -> String {
    match words.pop() {
        None => String::new(),
        Some(last) if words.is_empty() => last,
        Some(last) => format!("{} and {}", words.join(", "), last),
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    chars
        .next()
        .map_or(String::new(), |c| c.to_uppercase().chain(chars).collect())
}

/// Checks the SAYC advice for `hand` after `bids`, as plain text with one
/// line for each paragraph or table row.
#[cfg(test)]
fn check(bids: &[Bid], hand: &str, expect: Expect) {
    use std::str::FromStr;
    let advice = Convention::sayc().advice(bids, Cards::from_str(hand).unwrap());
    let html = format_as!(HTML, advice).into_string().replace("</th>", " ");
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
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 7 HCP, 7 LHCP and 7 SHCP, with 3♠ 3♥ 4♦ 3♣.
            You should bid, since a pass would show at most 5 high card points (you have 7).
            Suggested: 2♠ Single raise. It shows 3+ spades and 6–9 high card points. Your 7 high card points are near the bottom of that range.
            ♠ 2NT Jacoby 2NT: needs 4+ spades (you have 3) and 13+ high card points (you have 7).
            ♠ 3♠ Limit raise: needs 10–11 high card points (you have 7).
            ♠ 4♠ Natural game: it needs 8+ spades and 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21).
            ♦ 2♦ Two-level response: needs 10+ high card points (you have 7).
            ♣ 2♣ Two-level response: needs 10+ high card points (you have 7).
            NT 1NT Response: also fits, but the system prefers 2♠."#]],
    );
    check(
        &one_spade,
        "S 32 H 5432 D 432 C 5432",
        expect![[r#"
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 0 HCP, 0 LHCP and 1 SHCP, with 2♠ 4♥ 3♦ 4♣.
            Suggested: Pass Response pass. It shows at most 5 high card points.
            ♠ 2♠ Single raise: needs 3+ spades (you have 2) and 6–9 high card points (you have 0).
            ♠ 3♠ Limit raise: needs 3+ spades (you have 2) and 10–11 high card points (you have 0).
            ♥ 2♥ Two-level response: needs 5+ hearts (you have 4) and 10+ high card points (you have 0).
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
            ♠ 2♥ Jacoby transfer: needs 5+ spades (you have 4).
            ♠ 3♠ Natural: it needs 8+ spades and 24+ high card points between you; partner has shown at most 17 high card points, so that is out of reach.
            ♥ 2♦ Jacoby transfer: needs 5+ hearts (you have 4).
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
