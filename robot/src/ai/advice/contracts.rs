//! The contracts within reach, and how to bid toward them.
//!
//! The natural entries of a convention say what a partnership needs between
//! them for a contract, such as 26 high card points for 3NT.  Weighing that
//! against the hand and what partner has shown says whether the contract is
//! already sure, depends on partner, or is out of reach.  When it depends on
//! partner, what matters is what our next bid tells them: once they know we
//! have that much, they can bid the contract when they hold the rest.
use super::{
    describe, AddTo, Bound, Candidate, Line, List, Measure, Need, Reach, Shown, Strain, Text,
};
use crate::ai::{legal_bids, Convention};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};

/// How many contracts the advice lists, so that it fits on a screen.
const CONTRACTS: usize = 6;

/// A contract the partnership might reach.
struct Contract {
    bid: Bid,
    /// What the partnership needs between them for it.
    needs: HandValuation,
    reach: Reach,
}

impl Convention {
    /// The contracts worth bidding toward after `history`, in words, each
    /// with its strain.  In
    /// each strain where the partnership is likely to have a fit, it takes
    /// one contract within reach of each kind (part-score, game and so on):
    /// the highest that is sure, or else the cheapest.  It lists the highest
    /// kind that is sure and the next one up, but no slam until game is sure.
    /// `me` is what the hand's earlier bids have shown and `candidates` are
    /// the bids it could make now.
    pub(super) fn contracts(
        &self,
        history: &[Bid],
        hand: &HandValuation,
        partner: &Shown,
        me: &Shown,
        candidates: &[Candidate],
    ) -> Vec<(Strain, Line)> {
        let mut bids = history.to_vec();
        bids.push(Bid::Pass);
        let contracts: Vec<Contract> = legal_bids(history)
            .into_iter()
            .filter(|b| b.is_contract())
            .filter_map(|bid| {
                *bids.last_mut().unwrap() = bid;
                let needs = self.natural_needs(&bids)?;
                let reach = Reach::of(&needs, hand, partner);
                Some(Contract { bid, needs, reach })
            })
            .collect();

        let mut worth: Vec<&Contract> = Vec::new();
        for strain in Strain::all() {
            // Contracts come from `legal_bids`, so cheapest first.
            let mut kinds: Vec<&Contract> = Vec::new();
            for c in contracts.iter().filter(|c| {
                Strain::of(c.bid, &c.needs) == strain
                    && !matches!(c.reach, Reach::OutOfReach(_))
                    && likely_fit(c, hand, partner)
            }) {
                match kinds.last_mut() {
                    Some(k) if Kind::of(k.bid) == Kind::of(c.bid) => {
                        if matches!(c.reach, Reach::Sure) {
                            *k = c;
                        }
                    }
                    _ => kinds.push(c),
                }
            }
            let sure = kinds.iter().rposition(|c| matches!(c.reach, Reach::Sure));
            worth.extend(
                kinds
                    .into_iter()
                    .skip(sure.unwrap_or(0))
                    .take(2)
                    .filter(|c| sure.is_some() || Kind::of(c.bid) <= Kind::Game),
            );
        }
        worth.truncate(CONTRACTS);

        worth
            .into_iter()
            .map(|c| {
                let needs = describe(&c.needs, &HandValuation::MAX);
                let route = match c.reach {
                    Reach::IfPartnerHas(_) => route(c, me, partner, candidates),
                    _ => text!(),
                };
                let text = text!(
                    Kind::of(c.bid).name(),
                    ": needs ",
                    List::and(needs),
                    " between you; ",
                    c.reach.verdict(),
                    ".",
                    route
                );
                (Strain::of(c.bid, &c.needs), Line { bid: c.bid, text })
            })
            .collect()
    }

    /// What the first natural entry for the last bid of `bids` says the
    /// partnership needs between them.
    fn natural_needs(&self, bids: &[Bid]) -> Option<HandValuation> {
        match self {
            Convention::Ordered { conventions, .. } => conventions.iter().find_map(|c| match c {
                Convention::Natural { min, .. } if c.applies(bids) => Some(*min),
                _ => None,
            }),
            _ => None,
        }
    }
}

/// What the hand and partner have between them, as far as anyone knows.
pub(super) fn together(hand: &HandValuation, partner: &Shown) -> Option<Text> {
    let points = Measure::Hcp;
    let (lo, hi) = partner.range(points);
    let mut has: Vec<Bound> = Bound::new(points, hand.hcp + lo, hand.hcp.saturating_add(hi))
        .into_iter()
        .collect();
    for &suit in Suit::ALL.iter().rev() {
        let cards = hand.length[suit] + partner.min.length[suit];
        if cards >= 8 {
            has.extend(Bound::at_least(Measure::Length(suit), cards));
        }
    }
    if has.is_empty() {
        None
    } else {
        Some(text!("Together you have ", List::and(has), "."))
    }
}

/// Is the partnership likely to have the trumps `contract` needs?  Partner
/// is expected to have what they have shown in a suit, or an even share of
/// their other cards in a suit they have said nothing about, which is what
/// `adjust_for_earlier_bids` assumes too.
fn likely_fit(contract: &Contract, hand: &HandValuation, partner: &Shown) -> bool {
    let suit = match contract.bid {
        Bid::Suit(_, suit) => suit,
        _ => return true,
    };
    let shown = partner.min.length;
    let silent = Suit::ALL.iter().filter(|&&s| shown[s] == 0).count() as u8;
    let expected = if shown[suit] > 0 {
        shown[suit]
    } else {
        (13 - shown.sum()) / silent
    };
    hand.length[suit] + expected >= contract.needs.length[suit]
}

/// The kinds of contract, from the smallest.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Kind {
    PartScore,
    Game,
    SmallSlam,
    GrandSlam,
}

impl Kind {
    fn of(bid: Bid) -> Self {
        match bid {
            Bid::Suit(7, _) | Bid::NT(7) => Kind::GrandSlam,
            Bid::Suit(6, _) | Bid::NT(6) => Kind::SmallSlam,
            Bid::NT(level) if level >= 3 => Kind::Game,
            Bid::Suit(level, Suit::Hearts) | Bid::Suit(level, Suit::Spades) if level >= 4 => {
                Kind::Game
            }
            Bid::Suit(level, _) if level >= 5 => Kind::Game,
            _ => Kind::PartScore,
        }
    }
    fn name(self) -> &'static str {
        match self {
            Kind::PartScore => "part-score",
            Kind::Game => "game",
            Kind::SmallSlam => "small slam",
            Kind::GrandSlam => "grand slam",
        }
    }
}

/// How to bid toward a contract that depends on partner.  Among the bids in
/// `candidates` that fit the hand, it takes the one after which partner
/// needs the least beyond what they have shown, in points and in trumps
/// alike, and says what partner then knows and needs.
fn route(contract: &Contract, me: &Shown, partner: &Shown, candidates: &[Candidate]) -> Text {
    let fitting = candidates.iter().filter(|c| c.fits && c.bid != Bid::Pass);
    if fitting.clone().any(|c| c.bid == contract.bid) {
        return text!(" You can bid it now.");
    }
    // The robot's choice comes first, and wins a tie.
    let best = fitting
        .filter_map(|c| {
            let after = me.and(&c.meaning);
            let reach = Reach::of(&contract.needs, &after.least(), partner);
            let stretch = match &reach {
                Reach::Sure => 0,
                Reach::IfPartnerHas(needs) => needs.iter().map(Need::stretch).sum(),
                Reach::OutOfReach(_) => return None,
            };
            Some((stretch, c.bid, after, reach))
        })
        .min_by_key(|&(stretch, ..)| stretch);
    let (bid, after, reach) = match best {
        Some((_, bid, after, reach)) => (bid, after, reach),
        None => return text!(),
    };
    let knows: Vec<Bound> = Measure::all()
        .filter(|&m| m.of(&contract.needs) > 0)
        .filter_map(|m| Bound::at_least(m, m.of(&after.least())))
        .collect();
    if knows.is_empty() {
        return text!();
    }
    let then = match reach {
        Reach::IfPartnerHas(needs) => text!("so they can bid it with ", List::and(needs)),
        _ => text!("which is enough for them to bid it"),
    };
    let points = Measure::Hcp;
    if me.range(points).0 > 0 && after.range(points).0 > me.range(points).0 {
        text!(
            " Bidding ",
            bid,
            " shows more than you have so far: partner then knows you have ",
            List::and(knows),
            ", ",
            then,
            "."
        )
    } else {
        text!(
            " After ",
            bid,
            ", partner knows you have ",
            List::and(knows),
            ", ",
            then,
            "."
        )
    }
}
