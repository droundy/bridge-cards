//! The contracts within reach, and how to bid toward them.
//!
//! The natural entries of a convention say what a partnership needs between
//! them for a contract, such as 26 high card points for 3NT.  Weighing that
//! against the hand and what partner has shown says whether the contract is
//! already sure, depends on partner, or is out of reach.  When it depends on
//! partner, what matters is what our next bid tells them: once they know we
//! have that much, they can bid the contract when they hold the rest.
use super::{
    describe, partner_text, phrases, you, Bound, Candidate, Line, Measure, Need, Reach, Shown,
    Strain, KNOW,
};
use crate::ai::rules::{contract_needs, uncontested, Kind};
use crate::ai::{bids_string, legal_bids, Convention};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use wording::{and, Text, Word};

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
    /// with its strain.  In each strain where the partnership is likely to
    /// have a fit, it takes one contract within reach of each kind
    /// (part-score, game and so on):
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
                    && !matches!(c.reach, Reach::Beyond(_))
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
                    _ => Text::new(),
                };
                let text = Text::from(Kind::of(c.bid).name())
                    + ": needs"
                    + phrases(needs)
                    + "between you;"
                    + c.reach.verdict()
                    + "."
                    + route;
                (Strain::of(c.bid, &c.needs), Line { bid: c.bid, text })
            })
            .collect()
    }

    /// What the partnership needs between them for the last bid of `bids`:
    /// what the contract rules say for a game or slam, where they apply, and
    /// otherwise what the first natural entry for it says.
    fn natural_needs(&self, bids: &[Bid]) -> Option<HandValuation> {
        let auction = bids_string(bids);
        let candidates = self.candidates(&auction);
        let entries = self.entries();
        let ruled = candidates.iter().any(|&i| {
            matches!(&entries[i], Convention::Contract { regex, .. } if regex.is_match(&auction))
        });
        if ruled && uncontested(&auction) {
            if let Some(needs) = contract_needs(*bids.last()?) {
                return Some(needs);
            }
        }
        candidates.into_iter().find_map(|i| match &entries[i] {
            c @ Convention::Natural { min, .. } if c.applies(bids) => Some(*min),
            _ => None,
        })
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
        Some(Text::from("Together you have") + and(has.into_iter().map(Bound::text)) + ".")
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
        13u8.saturating_sub(shown.sum()) / silent
    };
    hand.length[suit] + expected >= contract.needs.length[suit]
}

/// How to bid toward a contract that depends on partner.  Among the bids in
/// `candidates` that fit the hand, it takes the one after which partner
/// needs the least beyond what they have shown, in points and in trumps
/// alike, and says what partner then knows and needs.
fn route(contract: &Contract, me: &Shown, partner: &Shown, candidates: &[Candidate]) -> Text {
    let fitting = candidates.iter().filter(|c| c.fits() && c.bid != Bid::Pass);
    if fitting.clone().any(|c| c.bid == contract.bid) {
        return Text::from("You can bid it now.");
    }
    // The robot's choice wins a tie, which is the entry that comes first.
    let best = fitting
        .filter_map(|c| {
            let after = me.and(&c.meaning);
            let reach = Reach::of(&contract.needs, &after.least(), partner);
            let stretch = match &reach {
                Reach::Sure => 0,
                Reach::IfPartnerHas(needs) => needs.iter().map(Need::stretch).sum(),
                Reach::Beyond(_) => return None,
            };
            Some((stretch, c.rank, c.bid, after, reach))
        })
        .min_by_key(|&(stretch, rank, ..)| (stretch, rank));
    let (bid, after, reach) = match best {
        Some((_, _, bid, after, reach)) => (bid, after, reach),
        None => return Text::new(),
    };
    let knows: Vec<Bound> = Measure::all()
        .filter(|&m| m.of(&contract.needs) > 0)
        .filter_map(|m| Bound::at_least(m, m.of(&after.least())))
        .collect();
    if knows.is_empty() {
        return Text::new();
    }
    let then = match reach {
        Reach::IfPartnerHas(needs) => {
            Text::from("so they can bid it with") + and(needs.into_iter().map(Need::text))
        }
        _ => Text::from("which is enough for them to bid it"),
    };
    let points = Measure::Hcp;
    let knows = you() + Word::HAS + and(knows.into_iter().map(Bound::text));
    if me.range(points).0 > 0 && after.range(points).0 > me.range(points).0 {
        Text::from("Bidding")
            + bid
            + "shows more than you have so far:"
            + (partner_text() + "then" + KNOW + knows)
            + ","
            + then
            + "."
    } else {
        Text::from("After") + bid + "," + (partner_text() + KNOW + knows) + "," + then + "."
    }
}

#[test]
fn likely_fit_copes_with_lengths_that_add_up_to_more_than_a_hand() {
    // Partner has shown 5+ spades, 5+ hearts and 4+ clubs, which is more
    // cards than a hand holds, and nothing in diamonds.
    let mut min = HandValuation::MIN;
    min.length[Suit::Spades] = 5;
    min.length[Suit::Hearts] = 5;
    min.length[Suit::Clubs] = 4;
    let partner = Shown {
        bids: Vec::new(),
        min,
        max: HandValuation::MAX,
    };
    let mut needs = HandValuation::MIN;
    needs.length[Suit::Diamonds] = 8;
    let contract = Contract {
        bid: Bid::Suit(3, Suit::Diamonds),
        needs,
        reach: Reach::Sure,
    };
    // Partner has no spare cards to expect in diamonds, so a hand with two of
    // them is not enough.
    assert!(!likely_fit(&contract, &HandValuation::MIN, &partner));
    let mut hand = HandValuation::MIN;
    hand.length[Suit::Diamonds] = 8;
    assert!(likely_fit(&contract, &hand, &partner));
}
