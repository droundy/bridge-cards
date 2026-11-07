//! Bidding advice: what the auction has shown so far and which bids suit a
//! hand, put into words from the same tables the robot bids by.
//!
//! Each bid means a box of bounds on a `HandValuation`, so the advice reads
//! every bound as a phrase such as "5+ spades" or "6–9 high card points".
//! When describing a bid it leaves out bounds that only shape a balanced hand
//! or break a tie with another bid, such as the "at most 5 hearts" of a 1S
//! opening.  The bid the robot would make is the first in `ranked_bids` that
//! fits the hand, and the other bids are explained by the bounds the hand
//! misses.  Natural bids state what the partnership needs between them, so
//! for those the advice weighs the hand against what partner has shown.
use super::{Convention, Forcing};
use crate::Bid;
use bridge_deck::{Cards, HandValuation, Suit};
use display_as::{format_as, with_template, DisplayAs, FormattedString, HTML};
#[cfg(test)]
use expect_test::{expect, Expect};

mod contracts;

/// Advice on the next bid of an auction.
pub(crate) struct Advice {
    /// What each player has shown, the hand, and whether it must bid.
    situation: Vec<FormattedString<HTML>>,
    /// The bid the robot would make, and why.
    suggestion: FormattedString<HTML>,
    /// The other bids closest to fitting, each with the suit it is about.
    others: Vec<(FormattedString<HTML>, FormattedString<HTML>)>,
    /// The contracts within reach, and how to bid toward them.
    contracts: Vec<FormattedString<HTML>>,
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

        let candidates: Vec<Candidate> = self
            .ranked_bids(history)
            .into_iter()
            .map(|(bid, meaning)| {
                let mut bids = history.to_vec();
                bids.push(bid);
                Candidate::new(bid, meaning, self.entry(&bids), &hand, &partner)
            })
            .collect();
        let chosen = candidates.iter().find(|c| c.fits);

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
            (_, Some(pass)) if !pass.fits => Some(format_as!(
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
            .filter(|c| c.bid != Bid::Pass || c.fits)
            .collect();
        others.sort_by_key(|c| (!c.fits, c.distance));
        others.truncate(OTHERS);
        others.sort_by_key(|c| (c.strain.order(), c.bid.level()));
        let others = others
            .into_iter()
            .map(|c| (c.strain.label(), c.comparison(chosen)))
            .collect();

        let contracts = self.contracts(
            history,
            &hand,
            &partner,
            &self.shown(history, 0),
            &candidates,
        );

        Advice {
            situation,
            suggestion,
            others,
            contracts,
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
            if let Some(Convention::Simple { min, max, .. }) = self.refine(&history[..=i]) {
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
        if let Convention::Simple { min, max, .. } = meaning {
            shown.min = shown.min.max(*min);
            shown.max = shown.max.min(*max);
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
    needs: Option<String>,
    /// For a natural bid, how what partner has shown bears on it.
    verdict: Option<String>,
    fits: bool,
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
    ) -> Self {
        let (min, max) = meaning.bounds();
        let (misses, distance) = misses(&min, &max, hand);
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
            verdict: together.map(|t| Reach::of(&t, hand, partner).verdict()),
            fits: meaning.fits(*hand),
            misses: join(misses),
            distance,
            strain: Strain::of(bid, &min),
            meaning,
        }
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
        let words = describe(&min, &max);
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
        if self.fits {
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

/// How what a partnership needs compares with a hand plus what partner has
/// shown.
enum Reach {
    /// The hand and the least partner has shown are enough.
    Sure,
    /// Enough if partner has these.
    IfPartnerHas(Vec<Need>),
    /// More than partner can have, given the most they have shown.
    OutOfReach(Vec<String>),
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
                out_of_reach.push(bound(m, 0, hi).unwrap_or_default());
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
    fn verdict(&self) -> String {
        match self {
            Reach::Sure => "you have that between you".to_string(),
            Reach::IfPartnerHas(needs) => {
                let needs = needs
                    .iter()
                    .map(|n| match range(n.m, n.shown.0, n.shown.1) {
                        Some(shown) => format!("{} (they have shown {})", n.words(), shown),
                        None => n.words(),
                    });
                format!("that works if partner has {}", join(needs.collect()))
            }
            Reach::OutOfReach(shown) => format!(
                "partner has shown {}, so that is out of reach",
                join(shown.clone())
            ),
        }
    }
}

/// Something partner needs to hold for a contract.
struct Need {
    m: Measure,
    /// How much of `m` partner needs.
    amount: u8,
    /// The range of `m` that partner has shown.
    shown: (u8, u8),
}

impl Need {
    fn words(&self) -> String {
        bound(self.m, self.amount, self.m.limit()).unwrap_or_default()
    }
    /// How far this asks beyond the least partner has shown, to compare the
    /// bids that might lead to a contract.
    fn stretch(&self) -> u32 {
        self.m.weight() * (self.amount - self.shown.0) as u32
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
    // Toward 4♠, 2♠ tells partner about the spade fit, which 1NT would not.
    check(
        &one_spade,
        "S Q32 H KJ4 D J543 C 654",
        expect![[r#"
            Bidding advice
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 7 HCP, 7 LHCP and 7 SHCP, with 3♠ 3♥ 4♦ 3♣.
            You should bid, since a pass would show at most 5 high card points (you have 7).
            Suggested: 2♠ Single raise (3+ card support). It shows 3+ spades and 6–9 high card points. Your 7 high card points are near the bottom of that range.
            ♠ 3♠ Limit raise (4+ card support): needs 4+ spades (you have 3) and 10–12 high card points (you have 7).
            ♠ 4♠ Natural game: it needs 8+ spades and 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21).
            ♦ 2♦ Two-level response (New suit): needs 10+ high card points (you have 7).
            ♣ 2♣ Two-level response (New suit): needs 4+ clubs (you have 3) and 10+ high card points (you have 7).
            NT 1NT Response (No fit and no suit to bid): also fits, but the system prefers 2♠.
            NT 3NT Natural game: it needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21).
            Possible contracts
            Together you have 20–28 high card points and 8+ spades.
            3♠ part-score: needs 8+ spades and 24+ high card points between you; that works if partner has 17+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 18+ high card points.
            4♠ game: needs 8+ spades and 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21). After 2♠, partner knows you have 3+ spades and 6+ high card points, so they can bid it with 20+ high card points.
            3NT game: needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21). After 2♠, partner knows you have 6+ high card points, so they can bid it with 20+ high card points."#]],
    );
    check(
        &one_spade,
        "S 32 H 5432 D 432 C 5432",
        expect![[r#"
            Bidding advice
            Partner opened 1♠, showing 5+ spades and 13–21 long high card points.
            Your hand: 0 HCP, 0 LHCP and 1 SHCP, with 2♠ 4♥ 3♦ 4♣.
            Suggested: Pass Response pass. It shows at most 5 high card points.
            ♠ 2♠ Single raise (3+ card support): needs 3+ spades (you have 2) and 6–9 high card points (you have 0).
            ♥ 2♥ Two-level response (New suit): needs 5+ hearts (you have 4) and 10+ high card points (you have 0).
            ♦ 2♦ Two-level response (New suit): needs 4+ diamonds (you have 3) and 10+ high card points (you have 0).
            ♣ 2♣ Two-level response (New suit): needs 10+ high card points (you have 0).
            NT 1NT Response (No fit and no suit to bid): needs 6–10 high card points (you have 0).
            NT 3NT Natural game: it needs 26+ high card points between you; partner has shown at most 21 high card points, so that is out of reach.
            Possible contracts
            Together you have 13–21 high card points."#]],
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
            Bidding advice
            Partner opened 1NT, showing a balanced hand and 15–17 high card points.
            Your hand: 6 HCP, 6 LHCP and 7 SHCP, with 4♠ 4♥ 2♦ 3♣.
            You should bid, since a pass would show at most 5 high card points (you have 6).
            No bid fits your hand, so the robot would pass.
            ♠ 2♥ Jacobi transfer: needs 5+ spades (you have 4).
            ♠ 3♠ Natural: it needs 8+ spades and 24+ high card points between you; partner has shown at most 17 high card points, so that is out of reach.
            ♥ 2♦ Jacobi transfer: needs 5+ hearts (you have 4).
            ♣ 2♣ Stayman (4-card major?): needs 8+ high card points (you have 6).
            NT 2NT Game invite: needs 8–10 high card points (you have 6).
            NT 3NT Natural game: it needs 26+ high card points between you; partner has shown at most 17 high card points, so that is out of reach.
            Possible contracts
            Together you have 21–23 high card points."#]],
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
            Bidding advice
            Partner bid 1NT, showing at most 3 spades and 6–10 high card points.
            You opened 1♥, showing 5+ hearts and 13–21 long high card points.
            Your hand: 19 HCP, 20 LHCP and 20 SHCP, with 2♠ 5♥ 3♦ 3♣.
            Suggested: 2NT Rebid (Balanced, 18-19 hcp). It shows a balanced hand and 18–19 high card points. Your 19 high card points are near the top of that range.
            ♥ 2♠ New suit (Reverse): needs 4+ spades (you have 2).
            ♥ 3♥ Jump rebid (6+ card suit, invitational): needs 6+ hearts (you have 5) and 17–18 long high card points (you have 20).
            ♥ 4♥ Natural game: it needs 8+ hearts and 26+ high card points between you; that works if partner has 3+ hearts and 7+ high card points (they have shown 6–10).
            ♦ 2♦ New suit: needs 4+ diamonds (you have 3).
            ♣ 2♣ New suit: needs 4+ clubs (you have 3).
            NT 3NT Natural game: it needs 26+ high card points between you; that works if partner has 7+ high card points (they have shown 6–10).
            Possible contracts
            Together you have 25–29 high card points.
            3♥ part-score: needs 8+ hearts and 24+ high card points between you; that works if partner has 3+ hearts. Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts.
            4♥ game: needs 8+ hearts and 26+ high card points between you; that works if partner has 3+ hearts and 7+ high card points (they have shown 6–10). Bidding 2NT shows more than you have so far: partner then knows you have 5+ hearts and 18+ high card points, so they can bid it with 3+ hearts and 8+ high card points.
            3NT game: needs 26+ high card points between you; that works if partner has 7+ high card points (they have shown 6–10). Bidding 2NT shows more than you have so far: partner then knows you have 18+ high card points, so they can bid it with 8+ high card points."#]],
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
            Bidding advice
            Partner opened 1♥, showing 5+ hearts and 13–21 long high card points.
            Your right-hand opponent bid 1♠, showing 5+ spades and 8–16 high card points.
            Your hand: 7 HCP, 7 LHCP and 8 SHCP, with 2♠ 4♥ 4♦ 3♣.
            Suggested: 3♥ Law of total tricks. It needs 9+ hearts between you, and you have that.
            ♥ 2♥ Law of total tricks: also fits, but the system prefers 3♥.
            ♥ 4♥ Law of total tricks: it needs 10+ hearts between you; that works if partner has 6+ hearts (they have shown 5+).
            ♦ 2♦ Law of total tricks: it needs 8+ diamonds between you; that works if partner has 4+ diamonds.
            ♦ 3♦ Law of total tricks: it needs 9+ diamonds between you; that works if partner has 5+ diamonds.
            ♣ 2♣ Law of total tricks: it needs 8+ clubs between you; that works if partner has 5+ clubs.
            NT 3NT Natural game: it needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21).
            Possible contracts
            Together you have 20–28 high card points and 9+ hearts.
            3♥ part-score: needs 9+ hearts between you; you have that between you.
            3NT game: needs 26+ high card points between you; that works if partner has 19+ high card points (they have shown 13–21)."#]],
    );
}
