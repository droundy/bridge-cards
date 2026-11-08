use crate::{bid_follows, Action, Bid, GameState};
use bridge_deck::{Card, Cards, HandValuation, Suit};
use regex::RegexSet;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

/// The bidding system a table plays.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum BiddingSystem {
    /// The house conventions, [`Convention::sheets`].
    #[default]
    Sheets,
    /// Standard American Yellow Card, [`Convention::sayc`].
    Sayc,
    /// Two-over-one game force, [`Convention::two_over_one`].
    TwoOverOne,
}

impl BiddingSystem {
    /// Every system, in the order they are offered to players.
    pub const ALL: [BiddingSystem; 3] = [
        BiddingSystem::Sheets,
        BiddingSystem::Sayc,
        BiddingSystem::TwoOverOne,
    ];
    /// The name shown to players.
    pub fn name(self) -> &'static str {
        match self {
            BiddingSystem::Sheets => "Sheets",
            BiddingSystem::Sayc => "SAYC",
            BiddingSystem::TwoOverOne => "2/1",
        }
    }
    /// The conventions this system bids by.
    pub fn convention(self) -> Convention {
        match self {
            BiddingSystem::Sheets => Convention::sheets(),
            BiddingSystem::Sayc => Convention::sayc(),
            BiddingSystem::TwoOverOne => Convention::two_over_one(),
        }
    }
}

#[derive(Debug)]
pub struct BridgeAi {
    bidder: ConventionalBid,
    player: RandomPlay,
}

impl BridgeAi {
    pub fn new() -> Self {
        Self::with_system(BiddingSystem::default())
    }
    /// A robot that bids according to `system`.
    pub fn with_system(system: BiddingSystem) -> Self {
        let bidder = ConventionalBid(system.convention());
        let player = RandomPlay;
        BridgeAi { bidder, player }
    }
    /// Decide which play to make.
    pub fn play<C>(&mut self, game: &GameState<C>) -> Action {
        if let Some(seat) = game.bidder() {
            Action::Bid(seat, self.bidder.bid(&game.bids, game.hands[seat]))
        } else {
            Action::Play(game.hand_playing().unwrap(), self.player.play(game))
        }
    }
}

#[wasm_bindgen]
pub fn choose_play(serialized_game: &str) -> Option<String> {
    crate::utils::set_panic_hook();
    let game = serde_json::from_str::<GameState<()>>(serialized_game).ok()?;
    let mut ai = BridgeAi::with_system(game.system);
    serde_json::to_string(&ai.play(&game)).ok()
}

pub trait BidAI: std::fmt::Debug {
    fn bid(&mut self, history: &[Bid], hand: Cards) -> Bid;
}
pub trait PlayAI: std::fmt::Debug {
    fn play<C>(&mut self, game: &GameState<C>) -> Card;
}

#[derive(Debug)]
pub struct AllPass;
impl BidAI for AllPass {
    fn bid(&mut self, _history: &[Bid], _hand: Cards) -> Bid {
        println!("Using AllPass AI");
        Bid::Pass
    }
}

/// The bids that may follow `history`, lowest priority first: Pass, then
/// Double, then contract bids from lowest to highest.
fn legal_bids(history: &[Bid]) -> Vec<Bid> {
    let mut candidates = vec![Bid::Pass, Bid::Double, Bid::Redouble];
    for level in 1..8 {
        for suit in Suit::ALL.iter().cloned() {
            candidates.push(Bid::Suit(level, suit));
        }
        candidates.push(Bid::NT(level));
    }
    candidates.retain(|&b| bid_follows(history, b));
    candidates
}

#[derive(Debug)]
pub struct ConventionalBid(pub Convention);
impl BidAI for ConventionalBid {
    fn bid(&mut self, history: &[Bid], hand: Cards) -> Bid {
        let legal_bids = legal_bids(history);
        let len = history.len();
        let mut bids = Vec::with_capacity(len + 1);
        bids.extend_from_slice(history);
        bids.push(Bid::Pass);
        for forcing_level in [
            Forcing::GameForcing,
            Forcing::Forcing,
            Forcing::Passable,
            Forcing::Forced,
        ]
        .iter()
        .cloned()
        {
            // First we check if we have a valid game-forcing bid, and pick the
            // highest such bid.  Otherwise we pick the highest valid forcing
            // bid.  If no such bid exists, we pick the highest valid passable
            // bid.
            for b in legal_bids.iter().cloned().rev() {
                bids[len] = b;
                if let Some(c) = self.0.refine(&bids) {
                    if c.is_forcing() == forcing_level {
                        let min = c.min_valuation(&bids);
                        let max = c.max_valuation(&bids);
                        let handvalue = hand.values();
                        if !handvalue.exceeds(max) && !min.exceeds(handvalue) {
                            println!(
                                "Bidding {:?} with convention {}",
                                b,
                                format_as!(HTML, c.description()).into_string()
                            );
                            println!("  hcp: {} < {} < {}", min.hcp, handvalue.hcp, max.hcp);
                            println!("  lhcp: {} < {} < {}", min.lhcp, handvalue.lhcp, max.lhcp);
                            return b;
                        }
                    }
                }
            }
        }
        Bid::Pass
    }
}

fn opening_lead(trump: Option<Suit>, hand: Cards) -> Card {
    // Heuristic for opening lead!
    if let Some(trump) = trump {
        let mut suits_to_consider = [Cards::EMPTY; 3];
        let mut goodness = [1000usize; 3];
        for (i, suit) in Suit::ALL
            .iter()
            .cloned()
            .filter(|&s| s != trump)
            .enumerate()
        {
            let cards = hand.in_suit(suit);
            suits_to_consider[i] = cards;
            if cards.is_empty() {
                // We can't lead a suit we don't have.
                goodness[i] = 0;
                continue;
            }

            if cards.len() == 1 {
                goodness[i] += 100;
            }
            let have_ace = cards.aces().non_empty();
            let have_king = cards.kings().non_empty();
            let have_queen = cards.queens().non_empty();
            let have_jack = cards.jacks().non_empty();
            let have_ten = cards.tens().non_empty();
            if have_ace && have_king {
                goodness[i] += 6;
            }
            if have_queen && have_king {
                goodness[i] += 4;
            }
            if have_queen && have_jack {
                goodness[i] += 3;
            }
            if have_ten && have_jack {
                goodness[i] += 2;
            }
            goodness[i] += 2 * cards.len();
            goodness[i] += cards.high_card_points();
            if have_ace && !have_king && cards.len() > 1 {
                goodness[i] -= 100;
            }
            if have_king && !have_ace && !have_queen && cards.len() > 1 {
                goodness[i] -= 7;
            }
        }
        let best_goodness = goodness.iter().cloned().max().unwrap();
        for i in (0..3).rev() {
            if goodness[i] == best_goodness && suits_to_consider[i].non_empty() {
                let cards = suits_to_consider[i];
                let have_ace = cards.aces().non_empty();
                let have_king = cards.kings().non_empty();
                let have_queen = cards.queens().non_empty();
                let have_jack = cards.jacks().non_empty();
                let have_ten = cards.tens().non_empty();
                if have_ace && have_king {
                    return cards.kings().next().unwrap();
                } else if ((have_king || have_jack) && have_queen) || (cards.len() == 2) {
                    return cards.rev().next().unwrap();
                } else if have_jack && have_ten {
                    return cards.jacks().next().unwrap();
                }
                // The normal advice is not to do something random, but
                return cards.clone().pick(1).unwrap().next().unwrap();
            }
        }
    } else {
        // NT: 4th down from longest and strongest suit
        let suits: Vec<Cards> = Suit::ALL
            .iter()
            .cloned()
            .map(|suit| hand.in_suit(suit))
            .collect();
        let lengths: Vec<usize> = suits
            .iter()
            .cloned()
            .map(|cards| cards.len() * 8 + cards.high_card_points())
            .collect();
        let max_length = lengths.iter().cloned().max().unwrap();
        for i in 0..4 {
            if lengths[i] == max_length {
                for (n, c) in suits[i].rev().enumerate() {
                    if n == 3 {
                        return c;
                    }
                }
            }
        }
    }
    hand.clone().pick(1).unwrap().next().unwrap()
}

#[test]
fn test_opening_lead() {
    use std::str::FromStr;
    assert_eq!(
        Card::C3,
        opening_lead(
            None,
            Cards::from_str("C: AKQ32 D: 234 H: 234 S: 23").unwrap()
        )
    );
    assert_eq!(
        Card::CK,
        opening_lead(
            Some(Suit::Hearts),
            Cards::from_str("C: AKQ32 D: 234 H: 234 S: 23").unwrap()
        )
    );
}

#[test]
fn opening_lead_with_a_void() {
    use std::str::FromStr;
    // Leading away from an ace is penalized, so the club void used to look
    // like the best suit to lead.
    let hand = Cards::from_str("S: 65432 H: A43 D: A5432").unwrap();
    let lead = opening_lead(Some(Suit::Spades), hand);
    assert!(hand.contains(lead));
    assert_eq!(Suit::Diamonds, lead.suit());

    // With nothing outside trumps, we have to lead a trump.
    let lead = opening_lead(Some(Suit::Spades), Cards::SPADES);
    assert!(Cards::SPADES.contains(lead));
}

#[derive(Debug)]
pub struct RandomPlay;
impl PlayAI for RandomPlay {
    fn play<C>(&mut self, game: &GameState<C>) -> Card {
        let starting = game.starting().expect("playing at the wrong time");
        let nt_or_trump = if let Some(Bid::Suit(_, trump)) = game.highest_contract_bid() {
            Some(trump)
        } else {
            None
        };
        println!("trump is {:?}", nt_or_trump);
        let tricks_left = game.hands.iter().map(|h| h.len()).max().unwrap();
        if game.played.len() % 4 == 0 && starting.hands[0].len() == 13 {
            opening_lead(nt_or_trump, starting.hands[0])
        } else if tricks_left < 14 && game.could_be_played().clone().len() > 1 {
            let cards_played: &[Card] = if game.played.len() == 4 {
                &[]
            } else {
                &game.played
            };
            let starting = game.starting().unwrap();
            for i in 0..4 {
                println!("   {}: {}", i, starting.hands[i]);
            }
            println!("   extra: {}", starting.unknown);
            starting.check();
            let mut solver = if tricks_left < 5 {
                bridge_solver::Naive::statistical(nt_or_trump, 16)
            } else if tricks_left < 6 {
                bridge_solver::Naive::statistical(nt_or_trump, 4)
            } else if tricks_left < 7 {
                bridge_solver::Naive::statistical(nt_or_trump, 2)
            } else if tricks_left < 8 {
                bridge_solver::Naive::statistical(nt_or_trump, 1)
                // bridge_solver::Naive::high_low(nt_or_trump)
            } else {
                // bridge_solver::Naive::statistical(nt_or_trump, 1)
                bridge_solver::Naive::oneround(nt_or_trump)
            };
            solver.score_after(starting, cards_played).1
        } else {
            // The follwoing should just unwrap, since there should always be a card to pick
            println!("could be played: {}", game.could_be_played());
            game.could_be_played()
                .pick(1)
                .unwrap()
                .next()
                .unwrap_or(Card::S2)
        }
    }
}

use display_as::{format_as, with_template, DisplayAs, FormattedString, HTML, UTF8};
mod sayc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum Forcing {
    Forced,
    Passable,
    Forcing,
    GameForcing,
}

#[with_template("" if *self == Forcing::Forcing { "<br/><strong>Forcing</strong>" } else if *self == Forcing::GameForcing { "<br/><strong>Game forcing</strong>" } )]
impl DisplayAs<HTML> for Forcing {}

// Limits of this representation, and changes that would lift them.
//
// The approximations listed at the top of `sayc.rs` come from three limits:
//
// 1. A bid is described by one box, a `min` and a `max` `HandValuation`.  A
//    box cannot compare suits ("spades at least as long as hearts"), cannot
//    say "this or that" ("at most two trumps, or 13+ hcp"), and cannot say
//    balanced: lengths of 2-5 in every suit let in 5-4-2-2.
// 2. Only one entry per auction counts.  `refine` takes the first entry whose
//    regex matches, so a later entry for the same auction is never seen, not
//    even as a fallback.
// 3. Choosing a bid is tied to forcing.  `ConventionalBid` tries the stronger
//    forcing levels first and, within a level, the highest bid that fits, so
//    how forcing a bid is also decides how much the robot prefers it.  That
//    is why SAYC needs bounds such as `hides_no_major`, and `Forced` entries
//    for the bids left when nothing else fits.
//
// Changes that would lift them, smallest first:
//
// 1. Shape constraints: an optional list beside the box, such as a suit at
//    least as long as another, a suit longer than another, truly balanced, or
//    an exact shape such as 4-4-3-2.  A small enum stays `Clone` and `Debug`
//    and can describe itself ("♠ ≥ ♥"), replacing descriptions that only say
//    so in text, as Sheets' `Spades ">" Hearts` does.  What partner infers
//    stays the box, so `adjust_for_earlier_bids` is unchanged, and so is
//    Sheets.  This would fix SAYC's 1D versus 1C, 5-5 and 6-5 hands, and
//    balanced notrump bids.
// 2. Alternatives within an entry: several boxes, of which the hand must fit
//    one.  What partner infers is their hull, which is no worse than today.
//    This would let Jacoby 2NT take four trumps again, beside a new suit with
//    three trumps and 13+, and give SAYC its 6-9 1NT and 10-11 limit raise.
// 3. Choosing bids in table order: let every entry for an auction count, and
//    bid the first one in the table that matches and fits, as a bridge book
//    reads ("with four trumps raise; else with a four card major you can show
//    at the one level, bid it; else ...").  Forcing then only says what a bid
//    means.  Fallbacks become the last entry, which replaces `hides_no_major`
//    and the `Forced` fallbacks, and covers what 1 and 2 cannot: up the line
//    with 4-4 majors, raising a minor with three only when 2NT does not fit,
//    and rebidding a five card suit when nothing else fits.  Descriptions and
//    `bid_understood2` would combine all entries for a bid.  Sheets could keep
//    today's rule by a per-system choice, checked with `dump_auctions`.
//
// Also: dummy points for raises (a `HandValuation` field for shortness, once
// the entry names the trump suit), which the booklet's raises are counted
// in; and, with table order, telling players that a bid denies every entry
// before it.
#[derive(Clone, Debug)]
pub enum Convention {
    Simple {
        regex: RegexSet,
        the_description: FormattedString<HTML>,
        the_name: &'static str,
        max: HandValuation,
        min: HandValuation,
        forcing: Forcing,
    },
    Natural {
        the_name: &'static str,
        regex: RegexSet,
        min: HandValuation,
        max: HandValuation,
    },
    Forced {
        the_name: &'static str,
        regex: RegexSet,
        min: HandValuation,
        max: HandValuation,
    },
    Ordered {
        conventions: Vec<Convention>,
        the_name: &'static str,
    },
}

fn bids_string(bids: &[Bid]) -> String {
    let mut s = String::with_capacity(3 * bids.len());
    use std::fmt::Write;
    for b in bids.iter() {
        match b {
            Bid::Double => s.push_str("X "),
            Bid::Redouble => s.push_str("XX "),
            Bid::Pass => s.push_str("P "),
            Bid::Suit(n, suit) => {
                write!(&mut s, "{}{} ", n, suit.latin()).unwrap();
            }
            Bid::NT(n) => {
                write!(&mut s, "{}N ", n).unwrap();
            }
        }
    }
    s.pop();
    s
}

/// The auction as it stood at each earlier bid by the same player (`skip` =
/// 4) or by their partner (`skip` = 2), most recent first.  Each prefix ends
/// with that earlier bid.
fn earlier_auctions(actual_bids: &[Bid], skip: usize) -> impl Iterator<Item = &[Bid]> {
    (skip..actual_bids.len())
        .step_by(4)
        .map(move |i| &actual_bids[..actual_bids.len() - i])
}

struct ValueRange {
    min: u8,
    max: u8,
    theoretical_max: u8,
    name: String,
}

#[with_template("[%" "%]" "value_range.html")]
impl DisplayAs<HTML> for ValueRange {}

impl ValueRange {
    fn new(min: u8, max: u8, theoretical_max: u8, name: &str) -> Self {
        ValueRange {
            max,
            min,
            theoretical_max,
            name: name.to_string(),
        }
    }
}

/// The length, hcp in suit, and hcp outside suit ranges for every suit.
fn suit_ranges(min: &HandValuation, max: &HandValuation) -> FormattedString<HTML> {
    format_as!(HTML,
        for suit in Suit::ALL.iter().cloned() {
            ValueRange::new(
                min.length[suit],
                max.length[suit],
                13,
                &format_as!(HTML, suit).into_string(),
            )
            ""
            ValueRange::new(
                min.hcp_in_suit[suit],
                max.hcp_in_suit[suit],
                10,
                &format_as!(HTML, "hcp in " suit).into_string(),
            )
            ""
            ValueRange::new(
                min.hcp_outside_suit[suit],
                max.hcp_outside_suit[suit],
                30,
                &format_as!(HTML, "hcp outside" suit).into_string(),
            )
        }
    )
}

impl Convention {
    fn min_valuation(&self, actual_bids: &[Bid]) -> HandValuation {
        match self.refine(actual_bids) {
            None => HandValuation::MIN,
            Some(Convention::Simple { min, .. }) => min,
            Some(Convention::Natural { min, .. }) => min,
            Some(Convention::Forced { min, .. }) => min,
            _ => unreachable!(),
        }
    }
    fn max_valuation(&self, actual_bids: &[Bid]) -> HandValuation {
        match self.refine(actual_bids) {
            None => HandValuation::MAX,
            Some(Convention::Simple { max, .. }) => max,
            Some(Convention::Natural { max, .. }) => max,
            Some(Convention::Forced { max, .. }) => max,
            _ => unreachable!(),
        }
    }
    fn check_forcing(&self, actual_bids: &[Bid]) -> Forcing {
        self.refine(actual_bids)
            .map_or(Forcing::Passable, |c| c.is_forcing())
    }
    pub fn refine(&self, actual_bids: &[Bid]) -> Option<Convention> {
        match self {
            Convention::Simple { regex, .. }
            | Convention::Natural { regex, .. }
            | Convention::Forced { regex, .. } => {
                if regex.is_match(&bids_string(actual_bids)) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            Convention::Ordered { conventions, .. } => {
                match conventions.iter().find(|c| c.applies(actual_bids)) {
                    Some(Convention::Natural {
                        the_name,
                        regex,
                        min,
                        max,
                    }) => {
                        let (min, max) = self.adjust_for_earlier_bids(actual_bids, *min, *max);
                        Some(Convention::Natural {
                            the_name,
                            regex: regex.clone(),
                            min,
                            max,
                        })
                    }
                    Some(Convention::Forced {
                        the_name,
                        regex,
                        min,
                        max,
                    }) => {
                        if self.partner_forcing(actual_bids) == Forcing::Passable {
                            return None;
                        }
                        let (min, max) = self.adjust_for_earlier_bids(actual_bids, *min, *max);
                        Some(Convention::Forced {
                            the_name,
                            regex: regex.clone(),
                            min,
                            max,
                        })
                    }
                    o => o.cloned(),
                }
            }
        }
    }
    /// How forcing partner's earlier bids were. A game force sticks once
    /// seen; otherwise the result is the forcing level of partner's first bid.
    fn partner_forcing(&self, actual_bids: &[Bid]) -> Forcing {
        let mut forcing = Forcing::Passable;
        for bids in earlier_auctions(actual_bids, 2) {
            if forcing != Forcing::GameForcing {
                forcing = self.check_forcing(bids);
            }
        }
        forcing
    }
    /// Reduce the `min` and `max` of a `Natural` or `Forced` bid by what
    /// partner and the bidder have already shown earlier in the auction.
    fn adjust_for_earlier_bids(
        &self,
        actual_bids: &[Bid],
        mut min: HandValuation,
        mut max: HandValuation,
    ) -> (HandValuation, HandValuation) {
        let mut partner = HandValuation::MIN;
        let mut partner_max = HandValuation::MAX;
        for bids in earlier_auctions(actual_bids, 2) {
            partner = partner.max(self.min_valuation(bids));
            partner_max = partner_max.min(self.max_valuation(bids));
        }
        let mut myself = HandValuation::MIN;
        for bids in earlier_auctions(actual_bids, 4) {
            myself = myself.max(self.min_valuation(bids));
        }
        // Partner's points count toward the requirement, and we bid this
        // with at most 2 hcp more than the reduced requirement.
        if min.hcp > 0 {
            let partner_hcp = std::cmp::max(partner.lhcp, partner.hcp);
            if partner_hcp < HandValuation::MAX.hcp {
                max.hcp = (min.hcp + 2).saturating_sub(partner_hcp);
            }
            min.hcp = min.hcp.saturating_sub(partner_hcp);
        }
        for suit in Suit::ALL.iter().cloned() {
            let partner_length = if partner.length[suit] == 0 && partner_max.length[suit] == 13 {
                // Partner has said nothing about this suit, so assume their
                // remaining cards are split evenly among the unmentioned suits.
                (13 - partner.length.sum())
                    / (4 - partner.length.iter().filter(|&&l| l > 0).count() as u8)
            } else {
                partner.length[suit]
            };
            min.length[suit] = min.length[suit].saturating_sub(partner_length);
            // We need no more length than we have already shown.
            if myself.length[suit] >= min.length[suit] {
                min.length[suit] = 0;
            }
        }
        (min, max)
    }
    pub fn refine2(&self, bid: Bid, otherbids: &[Bid]) -> Option<Convention> {
        let mut bids = otherbids.to_vec();
        bids.push(bid);
        self.refine(&bids)
    }
    /// Does this convention apply to the last bid in this sequence?
    pub fn applies(&self, actual_bids: &[Bid]) -> bool {
        self.refine(actual_bids).is_some()
    }
    /// A short description of the meaning of this bid.
    pub fn description(&self) -> impl DisplayAs<HTML> {
        match self {
            Convention::Simple {
                the_name,
                the_description,
                max,
                min,
                forcing,
                ..
            } => {
                format_as!(HTML,
                    "<strong>" the_name "</strong><br/>"
                ValueRange::new(min.hcp, max.hcp, HandValuation::MAX.hcp, "hcp") ""
                ValueRange::new(min.shcp, max.shcp, HandValuation::MAX.lhcp, "shcp") ""
                ValueRange::new(min.lhcp, max.lhcp, HandValuation::MAX.lhcp, "lhcp") ""
                suit_ranges(min, max)
                ""
                the_description
                ""
                forcing
                )
            }
            Convention::Natural {
                the_name, min, max, ..
            } => {
                format_as!(HTML,
                    "<strong>" the_name "</strong><br/>"
                    ValueRange::new(min.hcp, max.hcp, HandValuation::MAX.hcp, "hcp") ""
                    suit_ranges(min, max)
                )
            }
            Convention::Forced {
                the_name, min, max, ..
            } => {
                format_as!(HTML,
                    "<strong><i>" the_name "</i></strong><br/>"
                    ValueRange::new(min.hcp, max.hcp, HandValuation::MAX.hcp, "hcp") ""
                    suit_ranges(min, max)
                )
            }
            Convention::Ordered { the_name, .. } => {
                FormattedString::from_formatted(the_name.to_string())
            }
        }
    }
    /// Name of the convention
    pub fn name(&self) -> FormattedString<HTML> {
        match self {
            Convention::Simple { the_name, .. } => format_as!(HTML, the_name),
            Convention::Natural { the_name, .. } => format_as!(HTML, the_name),
            Convention::Forced { the_name, .. } => format_as!(HTML, the_name),
            Convention::Ordered { the_name, .. } => format_as!(HTML, the_name),
        }
    }
    /// Does this bid work for this hand?
    fn _is_appropriate(&self, _bids: &[Bid], _hand: Cards) -> bool {
        todo!()
    }

    /// How forcing is this bid?
    fn is_forcing(&self) -> Forcing {
        match self {
            Convention::Simple { forcing, .. } => *forcing,
            Convention::Natural { .. } => Forcing::Passable,
            Convention::Forced { .. } => Forcing::Forced,
            Convention::Ordered { .. } => Forcing::Passable,
        }
    }

    /// A new convention
    fn new(name: &'static str) -> Self {
        Convention::Ordered {
            the_name: name,
            conventions: Vec::new(),
        }
    }
    /// Add another possibility to the OrderedConvention
    fn add(&mut self, convention: Convention) {
        match self {
            Convention::Ordered { conventions, .. } => {
                conventions.push(convention);
            }
            Convention::Simple { .. } | Convention::Natural { .. } | Convention::Forced { .. } => {
                let s = self.clone();
                *self = Convention::Ordered {
                    conventions: vec![s, convention],
                    the_name: "",
                }
            }
        }
    }
}

impl Convention {
    /// Notrump openings with Stayman, Jacobi transfers and invitations.  The
    /// maximum `(hcp, shcp, lhcp)` for a 2NT opening is `two_nt_max`, and
    /// Stayman takes at least `stayman_hcp` points.
    fn add_notrump(&mut self, two_nt_max: (u8, u8, u8), stayman_hcp: u8) {
        let sheets = self;
        use bridge_deck::Suit::*;
        let max = HandValuation::MAX;
        let min = HandValuation::MIN;
        let mut min_nt = min;
        min_nt.length = [2, 2, 2, 2].into();
        let mut max_nt = max;
        max_nt.length = [5, 5, 5, 5].into();
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1N$"]).unwrap(),
            max: max_nt.with_hcp(17).with_shcp(18).with_lhcp(18),
            min: min_nt.with_hcp(15).with_shcp(15).with_lhcp(15),
            the_description: format_as!(HTML, ""),
            the_name: "Opening 1NT",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2N$"]).unwrap(),
            max: max_nt
                .with_hcp(two_nt_max.0)
                .with_shcp(two_nt_max.1)
                .with_lhcp(two_nt_max.2),
            min: min_nt.with_hcp(20).with_shcp(20).with_lhcp(20),
            the_description: format_as!(HTML, ""),
            the_name: "Opening 2NT",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1N [PX] 2N$"]).unwrap(),
            max: max.with_hcp(10),
            min: min.with_hcp(8),
            the_description: format_as!(HTML, ""),
            the_name: "Game invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1N [PX] 4N$"]).unwrap(),
            max: max.with_hcp(17),
            min: min.with_hcp(16),
            the_description: format_as!(HTML, ""),
            the_name: "Slam invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2N [PX] 3N$"]).unwrap(),
            max: max.with_hcp(10),
            min: min.with_hcp(5),
            the_description: format_as!(HTML, ""),
            the_name: "Game after 2N opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2|2N [PX] 3)C$"]).unwrap(),
            max,
            min: min.with_hcp(stayman_hcp),
            the_description: format_as!(HTML, "4-card major?"),
            the_name: "Stayman",
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)D$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 3, 3].into(),
                ..max
            },
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Stayman response",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 4, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Stayman response",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)S$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 3, 13].into(),
                ..max
            },
            min: HandValuation {
                length: [0, 0, 0, 4].into(),
                ..min
            },
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Stayman response",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2|2N [PX] 3)D$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Jacobi transfer",
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*(1N [PX] 2|2N [PX] 3)H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Jacobi transfer",
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([
                r"^(P )*1N [PX] 2D [PX] 2H$",
                r"^(P )*1N [PX] 2H [PX] 2S$",
                r"^(P )*2N [PX] 3D [PX] 3H$",
                r"^(P )*2N [PX] 3H [PX] 3S$",
            ])
            .unwrap(),
            max,
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Jacobi transfer response",
            forcing: Forcing::Passable,
        });
    }

    /// The strong artificial 2C opening, which shows `base` or more points,
    /// and its responses.
    fn add_strong_two_clubs(&mut self, base: u8) {
        let sheets = self;
        use bridge_deck::Suit::*;
        let max = HandValuation::MAX;
        let min = HandValuation::MIN;
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C$"]).unwrap(),
            max,
            min: min.with_lhcp(base),
            the_description: format_as!(HTML, "hcp≥" base),
            the_name: "Strong two",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D$"]).unwrap(),
            max: max.with_lhcp(7),
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Weak response",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 2S$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 2H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 3C$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            },
            min: HandValuation {
                length: [5, 0, 0, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 3D$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            },
            min: HandValuation {
                length: [0, 5, 0, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, Diamonds ">" Clubs),
            the_name: "Strong two rebid",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 2N$"]).unwrap(),
            max: max.with_lhcp(base + 1),
            min: min.with_lhcp(base),
            the_description: format_as!(HTML, ""),
            the_name: "Game invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D [PX] 3N$"]).unwrap(),
            max: max.with_lhcp(base + 4),
            min: min.with_lhcp(base + 2),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D ..? 4N$"]).unwrap(),
            max: max.with_lhcp(base + 7),
            min: min.with_lhcp(base + 5),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2D ..? 5N$"]).unwrap(),
            max: max.with_lhcp(base + 9),
            min: min.with_lhcp(base + 8),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                hcp_in_suit: [0, 0, 3, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong response",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2S$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                hcp_in_suit: [0, 0, 0, 3].into(),
                hcp: 8,
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong response",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 3C$"]).unwrap(),
            max,
            min: HandValuation {
                length: [5, 0, 0, 0].into(),
                hcp_in_suit: [3, 0, 0, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong response",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 3D$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 5, 0, 0].into(),
                hcp_in_suit: [0, 3, 0, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong response",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*2C [PX] 2N$"]).unwrap(),
            max: HandValuation {
                length: [5, 5, 5, 5].into(),
                ..max
            },
            min: HandValuation {
                hcp: 8,
                length: [2, 2, 2, 2].into(),
                ..min
            },
            the_description: format_as!(HTML, ""),
            the_name: "Strong response",
            forcing: Forcing::GameForcing,
        });
    }

    /// Weak twos in the red suits and spades, and weak threes.
    fn add_preempts(&mut self) {
        let sheets = self;
        use bridge_deck::Suit::*;
        let max = HandValuation::MAX;
        let min = HandValuation::MIN;
        for opening in [Diamonds, Hearts, Spades].iter().cloned() {
            let mut max = max.with_hcp(10).with_lhcp(12);
            max.length[opening] = 6;
            let mut min = min.with_hcp(5);
            min.length[opening] = 6;
            min.hcp_in_suit[opening] = 4;
            sheets.add(Convention::Simple {
                regex: RegexSet::new([&format!(r"^(P )?(P )?2{:?}$", opening)]).unwrap(),
                max,
                min,
                the_description: format_as!(HTML, ""),
                the_name: "Weak two",
                forcing: Forcing::Passable,
            });
        }
        for opening in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut max = max.with_hcp(9).with_lhcp(12);
            max.length[opening] = 7;
            let mut min = min.with_hcp(5);
            min.length[opening] = 7;
            min.hcp_in_suit[opening] = 4;
            sheets.add(Convention::Simple {
                regex: RegexSet::new([&format!(r"^(P )?(P )?3{:?}$", opening)]).unwrap(),
                max,
                min,
                the_description: format_as!(HTML, ""),
                the_name: "Weak three",
                forcing: Forcing::Passable,
            });
        }
    }

    /// Natural fallbacks for competitive and slam auctions.
    fn add_natural_bids(&mut self) {
        let sheets = self;
        use bridge_deck::Suit::*;
        let max = HandValuation::MAX;
        let min = HandValuation::MIN;
        for suit in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut min = min;
            min.length[suit] = 8;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new([&format!("[12]. [12]. .*2{:?}$", suit)]).unwrap(),
                max,
                min,
            });
            min.length[suit] = 9;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new([&format!("[123]. [123]. .*3{:?}$", suit)]).unwrap(),
                max,
                min,
            });
            min.length[suit] = 10;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new([&format!("[123]. [123]. .*4{:?}$", suit)]).unwrap(),
                max,
                min,
            });
        }

        sheets.add(Convention::Natural {
            the_name: "Natural game",
            regex: RegexSet::new(["3N$"]).unwrap(),
            min: min.with_hcp(26),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            the_name: "Natural",
            regex: RegexSet::new(["6N$"]).unwrap(),
            min: min.with_hcp(33),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            the_name: "Natural",
            regex: RegexSet::new(["7N$"]).unwrap(),
            min: min.with_hcp(37),
            max: HandValuation::MAX,
        });
        for major in [Hearts, Spades].iter().cloned() {
            let mut length = min.length;
            length[major] = 8;
            sheets.add(Convention::Natural {
                the_name: "Natural game",
                regex: RegexSet::new([&format!("4{:?}$", major)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 26,
                    length,
                    ..min
                },
            });
        }
        for minor in [Clubs, Diamonds].iter().cloned() {
            let mut length = min.length;
            length[minor] = 8;
            sheets.add(Convention::Natural {
                the_name: "Natural game",
                regex: RegexSet::new([&format!("5{:?}$", minor)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 29,
                    length,
                    ..min
                },
            });
        }
        for suit in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut length = min.length;
            length[suit] = 8;
            sheets.add(Convention::Natural {
                the_name: "Natural",
                regex: RegexSet::new([&format!("6{:?}$", suit)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 33,
                    length,
                    ..min
                },
            });
            sheets.add(Convention::Natural {
                the_name: "Natural",
                regex: RegexSet::new([&format!("7{:?}$", suit)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 37,
                    length,
                    ..min
                },
            });

            sheets.add(Convention::Natural {
                the_name: "Natural",
                regex: RegexSet::new([&format!("3{:?}$", suit)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 24,
                    length,
                    ..min
                },
            });
        }

        sheets.add(Convention::Forced {
            the_name: "Forced NT",
            regex: RegexSet::new(["1. P 1N$", "2. P 2N$"]).unwrap(),
            min: HandValuation::MIN,
            max: HandValuation::MAX,
        });
    }

    pub fn sheets() -> Self {
        let mut sheets = Convention::new("Sheets");
        use bridge_deck::Suit::*;
        let max = HandValuation::MAX;
        let max_no_5_major = HandValuation {
            length: [13, 13, 4, 4].into(),
            ..max
        };
        let min = HandValuation::MIN;

        for bid in Suit::ALL.iter().cloned() {
            let mut all_unbid = min.length;
            for o in Suit::ALL.iter().cloned().filter(|&o| o != bid) {
                all_unbid[o] = 4;
            }
            sheets.add(Convention::Simple {
                regex: RegexSet::new([&format!(r"^(P )?[123]{:?} X$", bid)]).unwrap(),
                max,
                min: HandValuation {
                    lhcp: 9,
                    length: all_unbid,
                    ..min
                },
                the_description: format_as!(HTML, ""),
                the_name: "Takeout double",
                forcing: Forcing::Forcing,
            });
            for response in Suit::ALL.iter().cloned() {
                if response != bid {
                    let level = if response > bid { 1 } else { 2 };
                    let mut length = min.length;
                    length[response] = 4;
                    sheets.add(Convention::Simple {
                        regex: RegexSet::new([
                            &format!(r"^(P )?1{:?} X P {}{:?}$", bid, level, response),
                            &format!(r"^(P )?2{:?} X P {}{:?}$", bid, level + 1, response),
                            &format!(r"^(P )?3{:?} X P {}{:?}$", bid, level + 2, response),
                        ])
                        .unwrap(),
                        max,
                        min: HandValuation { length, ..min },
                        the_description: format_as!(HTML, ""),
                        the_name: "Takeout double response",
                        forcing: Forcing::Passable,
                    });
                }

                let mut all_unbid = min.length;
                for o in Suit::ALL
                    .iter()
                    .cloned()
                    .filter(|&o| o != bid && o != response)
                {
                    all_unbid[o] = 4;
                }
                sheets.add(Convention::Simple {
                    regex: RegexSet::new([&format!(
                        r"^(P )?[123]{:?} P [123]{:?} X$",
                        bid, response
                    )])
                    .unwrap(),
                    max,
                    min: HandValuation {
                        lhcp: 9,
                        length: all_unbid,
                        ..min
                    },
                    the_description: format_as!(HTML, ""),
                    the_name: "Takeout double",
                    forcing: Forcing::Forcing,
                });

                for takeout_response in Suit::ALL
                    .iter()
                    .cloned()
                    .filter(|&t| t != bid && t != response)
                {
                    let level = if takeout_response > response { 1 } else { 2 };
                    let mut length = min.length;
                    length[response] = 4;
                    sheets.add(Convention::Simple {
                        regex: RegexSet::new([
                            &format!(
                                r"^(P )?1{:?} P 1{:?} X P {}{:?}$",
                                bid, response, level, takeout_response
                            ),
                            &format!(
                                r"^(P )?.{:?} P 2{:?} X P {}{:?}$",
                                bid,
                                response,
                                level + 1,
                                takeout_response,
                            ),
                            &format!(
                                r"^(P )?.{:?} P 3{:?} X P {}{:?}$",
                                bid,
                                response,
                                level + 2,
                                takeout_response,
                            ),
                        ])
                        .unwrap(),
                        max,
                        min: HandValuation { length, ..min },
                        the_description: format_as!(HTML, ""),
                        the_name: "Takeout double response",
                        forcing: Forcing::Passable,
                    });
                }
            }
        }

        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P P 1H$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 12,
                length: [0, 0, 5, 4].into(),
                ..min
            },
            the_description: format_as!(HTML, "15+ hcp + " Spades),
            the_name: "Rule of 15 opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P P 1S$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 11,
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: format_as!(HTML, "15+ hcp + " Spades),
            the_name: "Rule of 15 opening",
            forcing: Forcing::Passable,
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P P 1C$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 12,
                length: [3, 0, 0, 3].into(),
                ..min
            },
            the_description: format_as!(HTML, "15+ hcp + " Spades),
            the_name: "Rule of 15 opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P P 1D$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 12,
                length: [0, 3, 0, 3].into(),
                ..min
            },
            the_description: format_as!(HTML, "15+ hcp + " Spades),
            the_name: "Rule of 15 opening",
            forcing: Forcing::Passable,
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P 1H$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 12,
                length: [0, 0, 5, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, Hearts "≥" Spades),
            the_name: "Rule of 20 opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P 1S$"]).unwrap(),
            max,
            min: HandValuation {
                lhcp: 12,
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Rule of 20 opening",
            forcing: Forcing::Passable,
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P 1C$"]).unwrap(),
            max: max_no_5_major,
            min: HandValuation {
                lhcp: 12,
                length: [3, 0, 0, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, Clubs "≥" Diamonds),
            the_name: "Rule of 20 opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^P P 1D$"]).unwrap(),
            max: max_no_5_major,
            min: HandValuation {
                lhcp: 12,
                length: [0, 3, 0, 0].into(),
                ..min
            },
            the_description: format_as!(HTML, Diamonds ">" Clubs),
            the_name: "Rule of 20 opening",
            forcing: Forcing::Passable,
        });

        sheets.add(Convention::Simple {
            the_name: "Opening bid",
            regex: RegexSet::new([r"^(P )*1H$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "≥" Spades),
            max,
            min: HandValuation {
                lhcp: 13,
                length: [0, 0, 5, 0].into(),
                ..min
            },
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1S$"]).unwrap(),
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Opening bid",
            max,
            min: HandValuation {
                lhcp: 13,
                length: [0, 0, 0, 5].into(),
                ..min
            },
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1C$"]).unwrap(),
            the_description: format_as!(HTML, Clubs "≥" Diamonds),
            the_name: "Opening bid",
            max: max_no_5_major,
            min: HandValuation {
                lhcp: 13,
                length: [3, 0, 0, 0].into(),
                ..min
            },
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1D$"]).unwrap(),
            the_description: format_as!(HTML, Diamonds ">" Clubs),
            the_name: "Opening bid",
            max: max_no_5_major,
            min: HandValuation {
                lhcp: 13,
                length: [0, 3, 0, 0].into(),
                ..min
            },
            forcing: Forcing::Passable,
        });

        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new([r"^(P )*1. [PX] 1S$"]).unwrap(),
            the_description: format_as!(HTML, Spades ">" Hearts),
            max,
            min: HandValuation {
                hcp: 6,
                length: [0, 0, 0, 4].into(),
                ..min
            },
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new([r"^(P )*1. [PX] 1H$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "≥" Spades),
            max,
            min: HandValuation {
                hcp: 6,
                length: [0, 0, 4, 0].into(),
                ..min
            },
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new([r"^(P )*1. [PX] 1D$"]).unwrap(),
            the_description: format_as!(HTML, Diamonds ">" Clubs),
            max: HandValuation {
                length: [13, 13, 3, 3].into(),
                ..max
            },
            min: HandValuation {
                hcp: 6,
                length: [0, 4, 0, 0].into(),
                ..min
            },
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            the_name: "Up the line",
            regex: RegexSet::new([r"^(P )*1[CD] P 1[DH] [PX] 1S$"]).unwrap(),
            the_description: format_as!(HTML, ""),
            max: HandValuation {
                length: [13, 13, 3, 4].into(),
                ..max
            },
            min: HandValuation {
                length: [0, 0, 0, 4].into(),
                ..min
            },
            forcing: Forcing::Forcing,
        });

        for opening in [Hearts, Spades].iter().cloned() {
            let mut min_support = min;
            min_support.length[opening] = 3;
            let max = HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            };
            sheets.add(Convention::Simple {
                the_name: "Single raise",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 2{:?}$", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
                forcing: Forcing::Passable,
            });
            sheets.add(Convention::Simple {
                the_name: "Limit raise",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 3{:?}$", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(12),
                min: min_support.with_hcp(11),
                forcing: Forcing::Passable,
            });
        }
        for opening in [Clubs, Diamonds].iter().cloned() {
            let mut min_support = min;
            min_support.length[opening] = 5;
            let max = HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            };
            sheets.add(Convention::Simple {
                the_name: "Inverted minor weak raise",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 3{:?}$", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
                forcing: Forcing::Passable,
            });
            min_support.length[opening] = 4;
            sheets.add(Convention::Simple {
                the_name: "Inverted minor limit raise",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 2{:?}$", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(12),
                min: min_support.with_hcp(11),
                forcing: Forcing::Passable,
            });
        }
        for opening in [Hearts, Spades].iter().cloned() {
            let mut min_splinter = min;
            min_splinter.length[opening] = 4;
            for response in [Clubs, Diamonds, Hearts, Spades]
                .iter()
                .cloned()
                .filter(|s| *s != opening)
            {
                let mut max_splinter = max.with_hcp(12);
                max_splinter.length[response] = 1;
                max_splinter.hcp_in_suit[response] = 1;
                max_splinter.hcp_outside_suit[response] = 12;
                min_splinter.hcp_outside_suit[response] = 10;
                let splinterbid = if response > opening { 3 } else { 4 };
                sheets.add(Convention::Simple {
                    the_name: "Splinter",
                    regex: RegexSet::new([&format!(
                        "^(P )*1{:?} [PX] {}{:?}$",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    the_description: format_as!(HTML, ""),
                    max: max_splinter,
                    min: min_splinter,
                    forcing: Forcing::GameForcing,
                });
                if response < opening {
                    let mut min = min.with_hcp(11);
                    min.length[response] = 4;
                    sheets.add(Convention::Simple {
                        the_name: "2/1 response",
                        regex: RegexSet::new([&format!(
                            "^(P )*1{:?} [PX] 2{:?}$",
                            opening, response
                        )])
                        .unwrap(),
                        the_description: format_as!(HTML, ""),
                        max,
                        min,
                        forcing: Forcing::Forcing,
                    });
                }
            }

            let mut min_jacobi = min;
            min_jacobi.length[opening] = 4;
            sheets.add(Convention::Simple {
                the_name: "Jacobi 2NT",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 2N$", opening)]).unwrap(),
                the_description: format_as!(HTML, ""),
                max,
                min: min_jacobi.with_shcp(13),
                forcing: Forcing::GameForcing,
            });
        }
        for opening in [Clubs, Diamonds].iter().cloned() {
            let mut min_splinter = min;
            min_splinter.length[opening] = 4;
            min_splinter.hcp_outside_suit[opening] = 10;
            for response in [Diamonds, Hearts, Spades]
                .iter()
                .cloned()
                .filter(|s| *s != opening)
            {
                let mut max_splinter = max.with_hcp(12);
                max_splinter.length[Hearts] = 3;
                max_splinter.length[Spades] = 3;
                max_splinter.length[response] = 1;
                max_splinter.hcp_in_suit[response] = 1;
                max_splinter.hcp_outside_suit[response] = 12;
                let splinterbid = if response > opening { 3 } else { 4 };
                sheets.add(Convention::Simple {
                    the_name: "Splinter",
                    regex: RegexSet::new([&format!(
                        "^(P )*1{:?} [PX] {}{:?}$",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    max: max_splinter,
                    min: min_splinter,
                    the_description: format_as!(HTML, ""),
                    forcing: Forcing::GameForcing,
                });
            }
        }

        sheets.add(Convention::Simple {
            the_name: "Impossible response",
            regex: RegexSet::new([r"^(P )*1C [PX] 1N$"]).unwrap(),
            min: max,
            max,
            the_description: format_as!(HTML, "Do not bid!<br/>Bid 4 card suit instead!"),
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new([r"^(P )*1D [PX] 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 4, 3, 3].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: format_as!(HTML, ""),
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new([r"^(P )*1H [PX] 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 2, 3].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: format_as!(HTML, ""),
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new([r"^(P )*1S [PX] 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 13, 2].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: format_as!(HTML, ""),
            forcing: Forcing::Passable,
        });

        sheets.add_notrump((22, 23, 23), 0);

        sheets.add_strong_two_clubs(23);

        sheets.add_preempts();

        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*P$"]).unwrap(),
            max: max.with_lhcp(12),
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Opening pass",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new([r"^(P )*1. P P$"]).unwrap(),
            max: max.with_hcp(5),
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Response pass",
            forcing: Forcing::Passable,
        });

        // Overcall bids and responses!
        for opening in Suit::ALL.iter().cloned() {
            let (michaels_length, the_description) = match opening {
                Clubs | Diamonds => ([0, 0, 5, 5], format_as!(HTML, "")),
                Hearts => ([0, 0, 0, 5], format_as!(HTML, "5 in a minor")),
                Spades => ([0, 0, 5, 0], format_as!(HTML, "5 in a minor")),
            };
            sheets.add(Convention::Simple {
                the_name: "Michael's cuebid",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 2{:?}$", opening, opening)])
                    .unwrap(),
                max: max.with_hcp(12),
                min: HandValuation {
                    length: michaels_length.into(),
                    hcp: 9,
                    ..min
                },
                the_description,
                forcing: Forcing::Forcing,
            });

            sheets.add(Convention::Simple {
                the_name: "Unusual 2NT",
                regex: RegexSet::new([&format!("^(P )*1{:?} [PX] 2N$", opening)]).unwrap(),
                max: max.with_hcp(12),
                min: HandValuation {
                    length: match opening {
                        Clubs => [0, 5, 5, 0],
                        Diamonds => [5, 0, 5, 0],
                        Hearts | Spades => [5, 5, 0, 0],
                    }
                    .into(),
                    hcp: 9,
                    ..min
                },
                the_description: format_as!(HTML, "9-12 hcp or 17+ hcp"),
                forcing: Forcing::Forcing,
            });

            for overcall in Suit::ALL.iter().cloned().filter(|s| *s != opening) {
                let mut min_overcall = min.with_hcp(9);
                min_overcall.length[overcall] = 5;
                let bid = if overcall > opening { 1 } else { 2 };
                sheets.add(Convention::Simple {
                    the_name: "Direct overcall",
                    regex: RegexSet::new([&format!("^(P )*1{:?} {}{:?}$", opening, bid, overcall)])
                        .unwrap(),
                    max: max.with_hcp(17),
                    min: min_overcall,
                    the_description: format_as!(HTML, ""),
                    forcing: Forcing::Passable,
                });

                sheets.add(Convention::Simple {
                    the_name: "Overcall",
                    regex: RegexSet::new([&format!(
                        "^(P )*1{:?} P P {}{:?}$",
                        opening, bid, overcall
                    )])
                    .unwrap(),
                    max: max.with_hcp(17),
                    min: min_overcall,
                    the_description: format_as!(HTML, ""),
                    forcing: Forcing::Passable,
                });
            }

            for response in Suit::ALL.iter().cloned().filter(|s| *s != opening) {
                let responsebid = if response > opening { 1 } else { 2 };
                for overcall in Suit::ALL
                    .iter()
                    .cloned()
                    .filter(|s| *s != opening && *s != response)
                {
                    let bid = if overcall > response {
                        responsebid
                    } else {
                        responsebid + 1
                    };
                    let mut min_overcall = min.with_hcp(9);
                    min_overcall.length[overcall] = 5;
                    sheets.add(Convention::Simple {
                        the_name: "Overcall",
                        regex: RegexSet::new([&format!(
                            "^(P )*1{:?} P {}{:?} {}{:?}$",
                            opening, responsebid, response, bid, overcall
                        )])
                        .unwrap(),
                        max: max.with_hcp(17),
                        min: min_overcall,
                        the_description: format_as!(HTML, ""),
                        forcing: Forcing::Passable,
                    });
                }
            }
        }

        sheets.add_natural_bids();
        sheets
    }
}

#[test]
fn test_bidai() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sheets = ConventionalBid(Convention::sheets());
    let weak_hand = Cards::from_str("C 2345 D 234 H 234 S 234").unwrap();
    assert_eq!(
        Bid::NT(1),
        sheets.bid(&[Suit(1, Clubs), Pass, Suit(1, Hearts), Pass], weak_hand)
    );

    let nt_hand = Cards::from_str("C 23 D Q34 H AK4 S AK456").unwrap();
    assert_eq!(
        Bid::Suit(2, Spades),
        sheets.bid(&[NT(1), Pass, Suit(2, Clubs), Pass], nt_hand)
    );
}

#[test]
fn test_bidai_rebids_after_strong_two() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sheets = ConventionalBid(Convention::sheets());
    // Each of these opens 2C with 23 lhcp, and must not pass partner's 2D.
    for (hand, rebid) in [
        ("S AK3 H KQ4 D AQ5 C AJ32", NT(2)),
        ("S A3 H A4 D AQ5 C AQJ932", Suit(3, Clubs)),
        ("S A3 H A4 D AQJ932 C AQ5", Suit(3, Diamonds)),
    ] {
        let hand = Cards::from_str(hand).unwrap();
        assert_eq!(Suit(2, Clubs), sheets.bid(&[], hand));
        assert_eq!(
            rebid,
            sheets.bid(&[Suit(2, Clubs), Pass, Suit(2, Diamonds), Pass], hand)
        );
    }
}

#[test]
fn test_bidai_third_seat_rule_of_20() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sheets = ConventionalBid(Convention::sheets());
    // With 12 lhcp, third seat opens a five-card major.
    let spades = Cards::from_str("S AQ432 H 32 D KJ2 C J32").unwrap();
    assert_eq!(Suit(1, Spades), sheets.bid(&[Pass, Pass], spades));
    let hearts = Cards::from_str("S 32 H AQ432 D KJ2 C J32").unwrap();
    assert_eq!(Suit(1, Hearts), sheets.bid(&[Pass, Pass], hearts));
}

#[test]
fn test_sheets() {
    use bridge_deck::Suit::*;
    use Bid::*;
    let sheets = Convention::sheets();
    assert_eq!(None, sheets.refine(&[]).map(|c| c.name()));
    assert_eq!(
        Some(format_as!(HTML, "Opening pass")),
        sheets.refine(&[Pass]).map(|c| c.name())
    );
    assert_eq!(
        Some(format_as!(HTML, "Opening bid")),
        sheets.refine(&[Suit(1, Clubs)]).map(|c| c.name())
    );
    assert_eq!(
        None,
        sheets
            .refine(&[Suit(1, Clubs), Suit(1, Clubs)])
            .map(|c| c.name())
    );
}

/// Bid `deals` seeded deals with the robot in all four seats.  Each result is
/// the seed, the hands (dealer first) and the auction.
#[cfg(test)]
fn seeded_deals(convention: Convention, deals: u64) -> Vec<(u64, Vec<Cards>, Vec<Bid>)> {
    use rand::SeedableRng;
    let mut bidder = ConventionalBid(convention);
    let mut out = Vec::new();
    for seed in 0..deals {
        let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
        let mut deck = Cards::ALL;
        let mut hands = Vec::new();
        for _ in 0..3 {
            hands.push(deck.pick_rng(&mut rng, 13).unwrap());
        }
        hands.push(deck);
        let mut bids = Vec::new();
        while bids.len() < 4 || bids[bids.len() - 3..] != [Bid::Pass, Bid::Pass, Bid::Pass] {
            let bid = bidder.bid(&bids, hands[bids.len() % 4]);
            bids.push(bid);
            assert!(
                bids.len() < 60,
                "auction never ends: {}",
                bids_string(&bids)
            );
        }
        out.push((seed, hands, bids));
    }
    out
}

/// One line per deal with the seed and the auction.
#[cfg(test)]
fn seeded_auctions(convention: Convention, deals: u64) -> String {
    let mut out = String::new();
    for (seed, _, bids) in seeded_deals(convention, deals) {
        out.push_str(&format!("{} {}\n", seed, bids_string(&bids)));
    }
    out
}

/// Write the auctions of `AUCTION_DEALS` seeded deals to the file named by
/// `AUCTION_DUMP`, for the system named by `AUCTION_SYSTEM` (default Sheets),
/// so that a change to the conventions can be diffed against the old one.  Set
/// `AUCTION_HANDS` to include the hands.
#[test]
#[ignore]
fn dump_auctions() {
    let system = match std::env::var("AUCTION_SYSTEM").as_deref() {
        Ok("sayc") => BiddingSystem::Sayc,
        Ok("2/1") => BiddingSystem::TwoOverOne,
        _ => BiddingSystem::Sheets,
    };
    let deals = std::env::var("AUCTION_DEALS").map_or(2000, |n| n.parse().unwrap());
    let mut out = String::new();
    for (seed, hands, bids) in seeded_deals(system.convention(), deals) {
        out.push_str(&format!("{} {}\n", seed, bids_string(&bids)));
        if std::env::var("AUCTION_HANDS").is_ok() {
            for (i, hand) in hands.iter().enumerate() {
                let v = hand.values();
                out.push_str(&format!(
                    "    {} {} hcp {} lhcp {}\n",
                    i,
                    format_as!(UTF8, hand).into_string().replace('\n', " "),
                    v.hcp,
                    v.lhcp
                ));
            }
        }
    }
    std::fs::write(std::env::var("AUCTION_DUMP").unwrap(), out).unwrap();
}

#[test]
fn test_sayc_openings() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sayc = ConventionalBid(Convention::sayc());
    for (hand, opening) in [
        ("S AK3 H KQ4 D QJ5 C J932", NT(1)),
        ("S AQ432 H K32 D KJ2 C 32", Suit(1, Spades)),
        ("S 32 H AQ432 D KJ2 C K32", Suit(1, Hearts)),
        ("S AK3 H Q42 D KJ54 C 432", Suit(1, Diamonds)),
        ("S AK3 H Q42 D 543 C KJ54", Suit(1, Clubs)),
        ("S KQJ876 H 32 D 876 C 32", Suit(2, Spades)),
        ("S AK3 H AK4 D KQ5 C AQJ2", Suit(2, Clubs)),
        ("S 432 H K32 D Q32 C KJ32", Pass),
    ] {
        let hand = Cards::from_str(hand).unwrap();
        assert_eq!(opening, sayc.bid(&[], hand));
    }
}

#[test]
fn test_sayc_responses() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sayc = ConventionalBid(Convention::sayc());
    for (hand, response) in [
        ("S Q32 H K54 D J543 C 654", Suit(2, Spades)),
        ("S AJ43 H Q32 D K32 C 432", Suit(3, Spades)),
        ("S 32 H J32 D KJ43 C Q432", NT(1)),
        ("S 32 H AKJ32 D Q32 C 432", Suit(2, Hearts)),
        ("S 32 H 5432 D 432 C 5432", Pass),
    ] {
        let hand = Cards::from_str(hand).unwrap();
        assert_eq!(response, sayc.bid(&[Suit(1, Spades), Pass], hand));
    }
    // A four card major is shown before the diamonds.
    let hand = Cards::from_str("S 32 H KJ32 D Q432 C 432").unwrap();
    assert_eq!(Suit(1, Hearts), sayc.bid(&[Suit(1, Clubs), Pass], hand));
}

#[test]
fn test_sayc_opener_rebids() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sayc = ConventionalBid(Convention::sayc());
    // Opening 1H, partner responds 1S.
    for (hand, rebid) in [
        ("S K432 H AQ876 D 32 C K2", Suit(2, Spades)),
        ("S 32 H AQJ876 D K2 C K32", Suit(2, Hearts)),
        ("S 32 H AQ876 D KJ43 C K2", Suit(2, Diamonds)),
    ] {
        let hand = Cards::from_str(hand).unwrap();
        assert_eq!(Suit(1, Hearts), sayc.bid(&[], hand));
        assert_eq!(
            rebid,
            sayc.bid(&[Suit(1, Hearts), Pass, Suit(1, Spades), Pass], hand)
        );
    }
}

#[test]
fn test_two_over_one_differs_from_sayc() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut sayc = ConventionalBid(Convention::sayc());
    let mut two_over_one = ConventionalBid(Convention::two_over_one());
    let auction = [Suit(1, Spades), Pass];
    // Ten points with a five card suit: a two level response in SAYC, a
    // forcing notrump in two over one.
    let ten = Cards::from_str("S 32 H AKJ32 D Q32 C 432").unwrap();
    assert_eq!(Suit(2, Hearts), sayc.bid(&auction, ten));
    assert_eq!(NT(1), two_over_one.bid(&auction, ten));
    // With a game force both bid the suit.
    let fifteen = Cards::from_str("S 32 H AKQ32 D Q32 C KJ2").unwrap();
    assert_eq!(Suit(2, Hearts), sayc.bid(&auction, fifteen));
    assert_eq!(Suit(2, Hearts), two_over_one.bid(&auction, fifteen));
    // A two over one opener must not pass the forcing notrump.
    let minimum = Cards::from_str("S AJ432 H Q3 D K32 C K32").unwrap();
    assert_eq!(Suit(1, Spades), two_over_one.bid(&[], minimum));
    assert_eq!(
        Suit(2, Clubs),
        two_over_one.bid(&[Suit(1, Spades), Pass, NT(1), Pass], minimum)
    );
    assert_eq!(
        Pass,
        sayc.bid(&[Suit(1, Spades), Pass, NT(1), Pass], minimum)
    );
}

#[test]
fn test_major_raise_with_three_card_support() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    for (name, mut system) in [
        ("SAYC", ConventionalBid(Convention::sayc())),
        ("2/1", ConventionalBid(Convention::two_over_one())),
    ] {
        // Three card support and a minimum raises the major, even though
        // the two over one forcing notrump is also available.
        for (hand, opening, raise) in [
            ("S Q32 H KJ4 D J543 C 654", Spades, Suit(2, Spades)),
            ("S 432 H KJ2 D QJ43 C 654", Hearts, Suit(2, Hearts)),
        ] {
            let hand = Cards::from_str(hand).unwrap();
            assert_eq!(
                raise,
                system.bid(&[Suit(1, opening), Pass], hand),
                "{} over 1{:?}",
                name,
                opening
            );
        }
    }
}

#[test]
fn test_two_over_one_forcing_notrump_and_limit_raise() {
    use bridge_deck::Suit::*;
    use std::str::FromStr;
    use Bid::*;

    let mut two_over_one = ConventionalBid(Convention::two_over_one());
    let mut sayc = ConventionalBid(Convention::sayc());
    // Eleven points with a doubleton in the major still start with 1NT.
    for (hand, opening) in [
        ("S 32 H KQ4 D QJ543 C K54", Spades),
        ("S KJ4 H 32 D QJ543 C K54", Hearts),
    ] {
        let hand = Cards::from_str(hand).unwrap();
        assert_eq!(NT(1), two_over_one.bid(&[Suit(1, opening), Pass], hand));
    }
    // Three card support and a limit raise bids three of the major in both,
    // since the SAYC booklet's limit raise takes three trumps.
    let three = Cards::from_str("S KJ4 H Q32 D AJ43 C 654").unwrap();
    let auction = [Suit(1, Spades), Pass];
    assert_eq!(Suit(3, Spades), two_over_one.bid(&auction, three));
    assert_eq!(Suit(3, Spades), sayc.bid(&auction, three));
    // Four card support is a limit raise in both.
    let four = Cards::from_str("S KJ43 H Q32 D AJ4 C 654").unwrap();
    assert_eq!(Suit(3, Spades), two_over_one.bid(&auction, four));
    assert_eq!(Suit(3, Spades), sayc.bid(&auction, four));
}

#[test]
fn test_every_system_finishes_seeded_auctions() {
    for system in BiddingSystem::ALL.iter().cloned() {
        let auctions = seeded_auctions(system.convention(), 300);
        let passed_out = auctions.lines().filter(|l| l.ends_with("P P P P")).count();
        let first_round_pass = auctions
            .lines()
            .filter(|l| l.splitn(2, ' ').nth(1).unwrap().starts_with("P P P P"))
            .count();
        assert_eq!(passed_out, first_round_pass, "{:?}", system);
        // About one deal in eight is passed out when nobody has an opening.
        assert!(
            passed_out < 60,
            "{:?} passes out {} of 300",
            system,
            passed_out
        );
    }
}

#[test]
fn robot_bids_are_what_the_server_accepts() {
    use bridge_deck::Suit::*;
    use Bid::*;
    let mut every_bid = vec![Pass, Double, Redouble, Suit(0, Clubs), NT(0), NT(8)];
    for level in 1..8 {
        for suit in bridge_deck::Suit::ALL.iter().cloned() {
            every_bid.push(Suit(level, suit));
        }
        every_bid.push(NT(level));
    }
    for history in [
        vec![],
        vec![Pass, Pass],
        vec![Suit(1, Hearts)],
        vec![Suit(1, Hearts), Pass, Pass],
        vec![Suit(1, Hearts), Double],
        vec![Suit(1, Hearts), Double, Pass, Pass],
        vec![Suit(1, Hearts), Pass, NT(1), Pass],
        vec![NT(7)],
    ] {
        let mut game = GameState::<()>::new();
        game.bids = history.clone();
        let seat = game.bidder().unwrap();
        let accepted: Vec<Bid> = every_bid
            .iter()
            .cloned()
            .filter(|&b| game.bid_is_legal(seat, b))
            .collect();
        let mut robot = legal_bids(&history);
        assert!(robot.iter().all(|&b| game.bid_is_legal(seat, b)));
        robot.sort_by_key(|b| every_bid.iter().position(|x| x == b));
        assert_eq!(accepted, robot, "after {}", bids_string(&history));
    }
    assert!(legal_bids(&[Suit(1, Hearts), Pass, Pass]).contains(&Double));
    assert!(!legal_bids(&[Suit(1, Hearts), Double]).contains(&Double));
    assert!(!legal_bids(&[Suit(1, Hearts), Double]).contains(&Redouble));
    assert_eq!(legal_bids(&[NT(7)]), vec![Pass, Double]);
}

/// The bid `system` makes with `hand` after `auction`.
#[cfg(test)]
fn system_bid(system: BiddingSystem, auction: &[Bid], hand: &str) -> Bid {
    use std::str::FromStr;
    ConventionalBid(system.convention()).bid(auction, Cards::from_str(hand).unwrap())
}

#[test]
fn opener_rebids_after_a_two_level_response() {
    use bridge_deck::Suit::*;
    use Bid::*;

    for system in [BiddingSystem::Sayc, BiddingSystem::TwoOverOne] {
        for (auction, hand, rebid) in [
            // A new suit lower than the opening.
            (
                [Suit(1, Spades), Pass, Suit(2, Clubs), Pass],
                "S AQ432 H KJ32 D K32 C 3",
                Suit(2, Hearts),
            ),
            // A reverse.
            (
                [Suit(1, Hearts), Pass, Suit(2, Clubs), Pass],
                "S AKJ2 H AQ876 D K2 C K2",
                Suit(2, Spades),
            ),
            // A new suit at the three level.
            (
                [Suit(1, Spades), Pass, Suit(2, Hearts), Pass],
                "S AQ432 H 3 D K2 C KJ432",
                Suit(3, Clubs),
            ),
        ] {
            assert_eq!(
                rebid,
                system_bid(system, &auction, hand),
                "{:?} after {}",
                system,
                bids_string(&auction)
            );
        }
    }
}

#[test]
fn two_over_one_opener_bids_over_the_forcing_notrump() {
    use bridge_deck::Suit::*;
    use Bid::*;

    // A minimum 5-3-3-2 with two clubs bids its doubleton club in two over
    // one, and passes in SAYC, where 1NT is not forcing.
    let auction = [Suit(1, Spades), Pass, NT(1), Pass];
    let hand = "S AJ432 H K32 D Q32 C K2";
    assert_eq!(
        Suit(2, Clubs),
        system_bid(BiddingSystem::TwoOverOne, &auction, hand)
    );
    assert_eq!(Pass, system_bid(BiddingSystem::Sayc, &auction, hand));
}

#[test]
fn takeout_doubles_are_never_passed_for_want_of_a_suit() {
    use bridge_deck::Suit::*;
    use Bid::*;

    for system in [BiddingSystem::Sayc, BiddingSystem::TwoOverOne] {
        for (auction, hand, advance) in [
            (
                [Suit(1, Clubs), Double, Pass],
                "S 432 H 432 D 432 C 5432",
                Suit(1, Diamonds),
            ),
            (
                [Suit(1, Clubs), Double, Pass],
                "S 432 H Q432 D 432 C 432",
                Suit(1, Hearts),
            ),
            (
                [Suit(1, Spades), Double, Pass],
                "S 5432 H 432 D 432 C 432",
                Suit(2, Clubs),
            ),
        ] {
            assert_eq!(
                advance,
                system_bid(system, &auction, hand),
                "{:?} with {}",
                system,
                hand
            );
        }
    }
}

#[test]
fn sayc_raises_a_major_as_the_booklet_says() {
    use bridge_deck::Suit::*;
    use Bid::*;

    let auction = [Suit(1, Spades), Pass];
    for (hand, sayc, two_over_one) in [
        // A limit raise, with four trumps or three, rather than a new suit.
        ("S KJ43 H 32 D A32 C Q432", Suit(3, Spades), Suit(3, Spades)),
        ("S KJ4 H 32 D A32 C Q5432", Suit(3, Spades), Suit(3, Spades)),
        // A game force with three trumps.
        ("S KJ4 H 32 D AK32 C Q432", NT(2), Suit(2, Diamonds)),
    ] {
        assert_eq!(sayc, system_bid(BiddingSystem::Sayc, &auction, hand));
        assert_eq!(
            two_over_one,
            system_bid(BiddingSystem::TwoOverOne, &auction, hand)
        );
    }
    // Four trumps raise 1D.
    assert_eq!(
        Suit(2, Diamonds),
        system_bid(
            BiddingSystem::Sayc,
            &[Suit(1, Diamonds), Pass],
            "S 432 H 32 D KJ43 C Q432"
        )
    );
}

#[test]
fn sayc_opener_rebids_as_the_booklet_says() {
    use bridge_deck::Suit::*;
    use Bid::*;

    let sayc = Convention::sayc();
    let forcing = |auction: &[Bid]| {
        let description = format_as!(HTML, sayc.refine(auction).unwrap().description());
        description.into_string().contains("Forcing")
    };
    let one_level = [Suit(1, Diamonds), Pass, Suit(1, Hearts), Pass];
    // A new suit that is not a reverse does not force, after a one level
    // response, but everything forces after a two level response.
    assert!(!forcing(&[
        Suit(1, Diamonds),
        Pass,
        Suit(1, Hearts),
        Pass,
        Suit(2, Clubs)
    ]));
    assert!(!forcing(&[
        Suit(1, Diamonds),
        Pass,
        Suit(1, Hearts),
        Pass,
        Suit(1, Spades)
    ]));
    assert!(forcing(&[
        Suit(1, Diamonds),
        Pass,
        Suit(1, Spades),
        Pass,
        Suit(2, Hearts)
    ]));
    assert!(forcing(&[
        Suit(1, Spades),
        Pass,
        Suit(2, Clubs),
        Pass,
        Suit(2, Spades)
    ]));
    assert!(forcing(&[
        Suit(1, Spades),
        Pass,
        Suit(2, Clubs),
        Pass,
        Suit(2, Diamonds)
    ]));
    for (hand, rebid) in [
        ("S 3 H K32 D AQ432 C KJ32", Suit(2, Clubs)),
        // A four card spade suit is shown at the one level.
        ("S KJ32 H 3 D AQ43 C KJ32", Suit(1, Spades)),
        // 16 points with six diamonds is a jump rebid.
        ("S Q2 H 3 D AKJ432 C KJ3", Suit(3, Diamonds)),
    ] {
        assert_eq!(
            rebid,
            system_bid(BiddingSystem::Sayc, &one_level, hand),
            "{}",
            hand
        );
    }
    // A 1NT overcall is 15-18.
    assert_eq!(
        NT(1),
        system_bid(
            BiddingSystem::Sayc,
            &[Suit(1, Clubs)],
            "S AQ3 H KJ4 D Q54 C K432"
        )
    );
}
