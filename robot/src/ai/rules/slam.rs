//! Asking for aces and kings before a slam, as the booklet does: Gerber's 4C
//! directly over partner's balanced 1NT or 2NT, and otherwise Blackwood's
//! 4NT before a slam in a suit, when this side has not bid notrump.  Each
//! answers in steps, the first for none or all four, and then 5NT (after
//! Gerber, 5C) asks for kings.
//!
//! The robot asks when the contract rules would bid a slam on its points,
//! and needs an ace to ask for aces and a king to ask for kings, so that
//! the first step means none.  Once the aces are known it bids a small slam
//! with no more than one missing, and otherwise signs off at the five level
//! in a suit with a fit (after Gerber, also in 4NT); with all four, and the
//! points for a grand slam,
//! it asks for kings, and bids the grand only with all of them.  Partner
//! answers and leaves the contract to the one who asked.
use super::super::{all_bids, Convention, Fit, Forcing};
use super::{trump, uncontested, Kind};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use display_as::{format_as, HTML};
use regex::RegexSet;
use wording::Text;

/// The two ways of asking for aces.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ask {
    /// 4NT, then 5NT for kings.
    Blackwood,
    /// 4C over partner's 1NT or 2NT, then 5C for kings.
    Gerber,
}

impl Ask {
    /// The bid that asks for aces, or for kings.
    pub(super) fn bid(self, kings: bool) -> Bid {
        match (self, kings) {
            (Ask::Blackwood, false) => Bid::NT(4),
            (Ask::Blackwood, true) => Bid::NT(5),
            (Ask::Gerber, false) => Bid::Suit(4, Suit::Clubs),
            (Ask::Gerber, true) => Bid::Suit(5, Suit::Clubs),
        }
    }
    fn name(self) -> &'static str {
        match self {
            Ask::Blackwood => "Blackwood",
            Ask::Gerber => "Gerber",
        }
    }
}

/// The answers to `ask` in steps: none (or all four), one, two and three.
fn steps(ask: Bid) -> Vec<Bid> {
    all_bids()
        .into_iter()
        .filter(|b| b.is_contract())
        .skip_while(|&b| b != ask)
        .skip(1)
        .take(4)
        .collect()
}

/// An ask for aces or kings, and the slams it looks for: the robot asks
/// when the rules would bid one of them on its points.
#[derive(Clone, Debug)]
pub struct Asking {
    pub(crate) ask: Ask,
    pub(crate) kings: bool,
    slams: Vec<Convention>,
}

impl Asking {
    /// The ask in words.
    pub(crate) fn text(&self) -> Text {
        let cards = if self.kings { "kings" } else { "aces" };
        let steps = steps(self.ask.bid(self.kings));
        Text::from("Asks how many")
            + cards
            + "partner has:"
            + steps[0]
            + "shows none or four,"
            + steps[1]
            + "one,"
            + steps[2]
            + "two and"
            + steps[3]
            + "three."
    }
}

/// Does a hand ask?  It must hold an ace (for kings, a king), so that the
/// first step means none, and the rules must know a slam it looks for.
pub(crate) fn asking_fits(asking: &Asking, hand: &HandValuation) -> bool {
    let held = if asking.kings { hand.kings } else { hand.aces };
    held >= 1
        && asking
            .slams
            .iter()
            .any(|s| s.fits_when_nothing_explores(hand))
}

/// What this side has asked, and what partner has answered.
pub(super) struct Asked {
    /// Did the player now to bid ask?
    pub(super) by_me: bool,
    pub(super) ask: Ask,
    /// The aces and kings that the answers showed.
    pub(super) aces: Option<u8>,
    pub(super) kings: Option<u8>,
    /// Has the player now to bid just been asked?
    pending: Option<Bid>,
}

/// A table entry made on the spot, which matches no auction of its own.
fn entry(name: &'static str, description: &str, fits: Vec<Fit>) -> Convention {
    Convention::Rule {
        regex: RegexSet::empty(),
        the_description: format_as!(HTML, description),
        the_name: name,
        fits,
        forcing: Forcing::Passable,
    }
}

impl Convention {
    /// What this side has asked about aces and kings before the next bid
    /// after `history`, if anything.
    pub(super) fn asked(&self, history: &[Bid]) -> Option<Asked> {
        let n = history.len();
        let mut asked: Option<Asked> = None;
        // This side's calls, from the first.
        for i in (0..n).filter(|i| (n - i).is_multiple_of(2)) {
            let Some(Convention::SlamAsk { asking, .. }) = self.refine(&history[..=i]) else {
                continue;
            };
            let answer = (history.get(i + 1) == Some(&Bid::Pass))
                .then(|| history.get(i + 2))
                .flatten()
                .and_then(|a| steps(history[i]).iter().position(|b| b == a))
                .map(|step| step as u8);
            let state = asked.get_or_insert(Asked {
                by_me: (n - i).is_multiple_of(4),
                ask: asking.ask,
                aces: None,
                kings: None,
                pending: None,
            });
            if asking.kings {
                state.kings = answer;
            } else {
                state.aces = answer;
            }
            if i + 2 == n && history[n - 1] == Bid::Pass {
                state.pending = Some(history[i]);
            }
        }
        asked
    }

    /// The bid that would ask for aces before a slam in `strain`, if there
    /// is one: Gerber directly over partner's balanced 1NT or 2NT, and
    /// otherwise Blackwood for a slam in a suit, unless this side has bid
    /// notrump, when 4NT invites one.
    pub(super) fn ace_ask(&self, history: &[Bid], strain: Option<Suit>) -> Option<Ask> {
        let n = history.len();
        let over_notrump = n >= 2
            && matches!(history[n - 2], Bid::NT(1) | Bid::NT(2))
            && history[n - 1] == Bid::Pass
            && self
                .refine(&history[..n - 1])
                .is_some_and(|m| m.bounds().0.length.iter().all(|&l| l >= 2));
        let ask = if over_notrump {
            Ask::Gerber
        } else if strain.is_none() {
            return None;
        } else {
            let notrump = history
                .iter()
                .rev()
                .skip(1)
                .step_by(2)
                .any(|b| matches!(b, Bid::NT(_)));
            (!notrump).then_some(Ask::Blackwood)?
        };
        let mut bids = history.to_vec();
        bids.push(ask.bid(false));
        (super::super::legal_bids(history).contains(&ask.bid(false)) && !self.conventional(&bids))
            .then_some(ask)
    }

    /// What the slam asks make of the last of `bids`, when they decide it:
    /// `Some(None)` when the bid means nothing, as any bid but an answer
    /// does once partner has asked.
    pub(super) fn slam_meaning(&self, bids: &[Bid]) -> Option<Option<Convention>> {
        let (&bid, history) = bids.split_last()?;
        let asked = self.asked(history)?;
        if let Some(ask) = asked.pending {
            let kings = ask == asked.ask.bid(true);
            let Some(step) = steps(ask).iter().position(|&b| b == bid) else {
                return Some(None);
            };
            let step = step as u8;
            let what = if kings { "king" } else { "ace" };
            let count = |n: u8| {
                let held = if kings {
                    HandValuation::MIN.with_kings(n)
                } else {
                    HandValuation::MIN.with_aces(n)
                };
                let most = if kings {
                    HandValuation::MAX.with_kings(n)
                } else {
                    HandValuation::MAX.with_aces(n)
                };
                Fit::new(held, most)
            };
            let (description, fits) = match step {
                0 => (format!("No {what}s or four"), vec![count(0), count(4)]),
                1 => (format!("One {what}"), vec![count(1)]),
                n => (format!("{n} {what}s"), vec![count(n)]),
            };
            let name = if kings { "King answer" } else { "Ace answer" };
            return Some(Some(entry(name, &description, fits)));
        }
        if !asked.by_me {
            // Partner has asked, and places the contract.
            return Some(None);
        }
        let aces = asked.aces?;
        if asked.kings.is_none() && bid == asked.ask.bid(true) {
            return Some(self.asking(history, asked.ask, true));
        }
        // With two aces missing, the asker signs off below the slam.
        let missing = HandValuation::MAX.with_aces(2u8.checked_sub(aces)?);
        let partner = self.promise(bids, 2);
        let sign_off = match (asked.ask, bid) {
            (Ask::Gerber, Bid::NT(4)) => Some(Fit::new(HandValuation::MIN, missing)),
            (_, Bid::Suit(5, suit)) => {
                let mut fit = HandValuation::MIN;
                fit.length[suit] = 8u8.saturating_sub(partner.min.length[suit]);
                Some(Fit::new(fit, missing))
            }
            _ => None,
        };
        sign_off.map(|fit| Some(entry("Sign-off", "Two aces are missing", vec![fit])))
    }

    /// The ask for aces (or kings) after `history` with `ask`, which looks
    /// for the slams the rules would bid without it.
    pub(super) fn asking(&self, history: &[Bid], ask: Ask, kings: bool) -> Option<Convention> {
        let mut bids = history.to_vec();
        bids.push(ask.bid(kings));
        if !uncontested(&super::bids_string(&bids)) {
            return None;
        }
        let levels: &[usize] = if kings { &[7] } else { &[6, 7] };
        let mut strains: Vec<Option<Suit>> = Suit::ALL.iter().map(|&s| Some(s)).collect();
        if ask == Ask::Gerber {
            strains.push(None);
        }
        let mut slams = Vec::new();
        for &strain in &strains {
            for &level in levels {
                let slam = match strain {
                    Some(suit) => Bid::Suit(level, suit),
                    None => Bid::NT(level),
                };
                let mut auction = history.to_vec();
                auction.push(slam);
                if let Some(Convention::Contracted {
                    the_name,
                    min,
                    max,
                    reason,
                    mut choice,
                }) = self.refine(&auction)
                {
                    debug_assert!(trump(slam) == strain && Kind::of(slam) >= Kind::SmallSlam);
                    choice.ask_first = None;
                    slams.push(Convention::Contracted {
                        the_name,
                        min,
                        max,
                        reason,
                        choice,
                    });
                }
            }
        }
        Some(Convention::SlamAsk {
            the_name: if kings { "King ask" } else { ask.name() },
            asking: Asking { ask, kings, slams },
        })
    }
}
