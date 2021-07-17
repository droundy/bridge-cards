use crate::{Bid, GameState};
use bridge_deck::{Card, Cards, HandValuation, Suit};
use regex::RegexSet;
pub trait BidAI: std::fmt::Debug {
    fn bid(&mut self, history: &[Bid], hand: Cards) -> Bid;
}
pub trait PlayAI: std::fmt::Debug {
    fn play(&mut self, game: &GameState) -> Card;
}

#[derive(Debug)]
pub struct AllPass;
impl BidAI for AllPass {
    fn bid(&mut self, _history: &[Bid], _hand: Cards) -> Bid {
        println!("Using AllPass AI");
        Bid::Pass
    }
}

#[derive(Debug)]
pub struct ConventionalBid(pub Convention);
impl BidAI for ConventionalBid {
    fn bid(&mut self, history: &[Bid], hand: Cards) -> Bid {
        let mut legal_bids = vec![Bid::Pass];
        let len = history.len();
        if history.len() > 0 && history[len - 1].is_contract() {
            legal_bids.push(Bid::Double);
        } else if history.len() > 2
            && history[len - 1] == Bid::Pass
            && history[len - 2] == Bid::Pass
            && history[len - 3].is_contract()
        {
            legal_bids.push(Bid::Double);
        } else if history.len() > 0 && history[len - 1] == Bid::Double {
            legal_bids.push(Bid::Redouble);
        } else if history.len() > 2
            && history[len - 1] == Bid::Pass
            && history[len - 2] == Bid::Pass
            && history[len - 3] == Bid::Double
        {
            legal_bids.push(Bid::Redouble);
        }
        let last_contract = history
            .iter()
            .cloned()
            .rev()
            .filter(|b| b.is_contract())
            .next()
            .unwrap_or(Bid::NT(0));
        for level in 1..8 {
            for suit in Suit::ALL.iter().cloned() {
                let bid = Bid::Suit(level, suit);
                if bid > last_contract {
                    legal_bids.push(bid);
                }
            }
            if Bid::NT(level) > last_contract {
                legal_bids.push(Bid::NT(level));
            }
        }
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
            if goodness[i] == best_goodness {
                let cards = suits_to_consider[i];
                let have_ace = cards.aces().non_empty();
                let have_king = cards.kings().non_empty();
                let have_queen = cards.queens().non_empty();
                let have_jack = cards.jacks().non_empty();
                let have_ten = cards.tens().non_empty();
                if have_ace && have_king {
                    return cards.kings().next().unwrap();
                } else if (have_king && have_queen)
                    || (have_queen && have_jack)
                    || (cards.len() == 2)
                {
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

#[derive(Debug)]
pub struct RandomPlay;
impl PlayAI for RandomPlay {
    fn play(&mut self, game: &GameState) -> Card {
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
                bridge_solver::Naive::statistical(nt_or_trump, 32)
            } else if tricks_left < 7 {
                bridge_solver::Naive::statistical(nt_or_trump, 8)
            } else if tricks_left < 9 {
                bridge_solver::Naive::statistical(nt_or_trump, 2)
            } else if tricks_left < 10 {
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum Forcing {
    Forced,
    Passable,
    Forcing,
    GameForcing,
}

#[with_template("" if *self == Forcing::Forcing { "<br/><strong>Forcing</strong>" } else if *self == Forcing::GameForcing { "<br/><strong>Game forcing</strong>" } )]
impl DisplayAs<HTML> for Forcing {}

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
            Convention::Simple { regex, .. } => {
                if regex.is_match(&bids_string(actual_bids)) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            Convention::Natural { regex, .. } => {
                if regex.is_match(&bids_string(actual_bids)) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            Convention::Forced { regex, .. } => {
                if regex.is_match(&bids_string(actual_bids)) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            Convention::Ordered { conventions, .. } => {
                match conventions.iter().filter(|c| c.applies(actual_bids)).next() {
                    Some(Convention::Natural {
                        the_name,
                        regex,
                        min,
                        max,
                    }) => {
                        let mut partner = HandValuation::MIN;
                        let mut partner_max = HandValuation::MAX;
                        let mut max = *max;
                        for i in (2..actual_bids.len()).step_by(4) {
                            let bids = &actual_bids[0..actual_bids.len() - i];
                            partner = partner.max(self.min_valuation(bids));
                            partner_max = partner_max.min(self.max_valuation(bids));
                        }
                        let mut myself = HandValuation::MIN;
                        for i in (4..actual_bids.len()).step_by(4) {
                            let bids = &actual_bids[0..actual_bids.len() - i];
                            myself = myself.max(self.min_valuation(bids));
                        }
                        let mut min = *min;
                        let regex = regex.clone();
                        if min.hcp > 0 {
                            let partner_hcp = std::cmp::max(partner.lhcp, partner.hcp);
                            if partner_hcp < HandValuation::MAX.hcp {
                                max.hcp = (min.hcp + 2).saturating_sub(partner_hcp);
                            }
                            min.hcp = min.hcp.saturating_sub(partner_hcp);
                        }
                        for suit in Suit::ALL.iter().cloned() {
                            // If partner hasn't said anything about
                            let partner_length = if partner.length[suit] == 0
                                && partner_max.length[suit] == 13
                            {
                                // Assume equally divided suits by partner if they haven't mentioned a suit
                                (13 - partner.length.sum())
                                    / (4 - partner.length.iter().filter(|&&l| l > 0).count() as u8)
                            } else {
                                partner.length[suit]
                            };
                            min.length[suit] = min.length[suit].saturating_sub(partner_length);
                            if myself.length[suit] >= min.length[suit] {
                                min.length[suit] = 0;
                            }
                        }
                        Some(Convention::Natural {
                            the_name: *the_name,
                            regex,
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
                        let mut partner = HandValuation::MIN;
                        let mut partner_max = HandValuation::MAX;
                        let mut max = *max;
                        let mut is_forcing = Forcing::Passable;
                        for i in (2..actual_bids.len()).step_by(4) {
                            let bids = &actual_bids[0..actual_bids.len() - i];
                            if is_forcing != Forcing::GameForcing {
                                is_forcing = self.check_forcing(bids);
                            }
                            partner = partner.max(self.min_valuation(bids));
                            partner_max = partner_max.min(self.max_valuation(bids));
                        }
                        if is_forcing == Forcing::Passable {
                            return None;
                        }
                        let mut myself = HandValuation::MIN;
                        for i in (4..actual_bids.len()).step_by(4) {
                            let bids = &actual_bids[0..actual_bids.len() - i];
                            myself = myself.max(self.min_valuation(bids));
                        }
                        let mut min = *min;
                        let regex = regex.clone();
                        if min.hcp > 0 {
                            let partner_hcp = std::cmp::max(partner.lhcp, partner.hcp);
                            if partner_hcp < HandValuation::MAX.hcp {
                                max.hcp = (min.hcp + 2).saturating_sub(partner_hcp);
                            }
                            min.hcp = min.hcp.saturating_sub(partner_hcp);
                        }
                        for suit in Suit::ALL.iter().cloned() {
                            // If partner hasn't said anything about
                            let partner_length = if partner.length[suit] == 0
                                && partner_max.length[suit] == 13
                            {
                                // Assume equally divided suits by partner if they haven't mentioned a suit
                                (13 - partner.length.sum())
                                    / (4 - partner.length.iter().filter(|&&l| l > 0).count() as u8)
                            } else {
                                partner.length[suit]
                            };
                            min.length[suit] = min.length[suit].saturating_sub(partner_length);
                            if myself.length[suit] >= min.length[suit] {
                                min.length[suit] = 0;
                            }
                        }
                        Some(Convention::Forced {
                            the_name: *the_name,
                            regex,
                            min,
                            max,
                        })
                    }
                    o => o.cloned(),
                }
            }
        }
    }
    pub fn refine2(&self, bid: Bid, otherbids: &[Bid]) -> Option<Convention> {
        let mut bids = otherbids.iter().cloned().collect::<Vec<_>>();
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
            Convention::Forced {
                the_name, min, max, ..
            } => {
                format_as!(HTML,
                    "<strong><i>" the_name "</i></strong><br/>"
                    ValueRange::new(min.hcp, max.hcp, HandValuation::MAX.hcp, "hcp") ""
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
                regex: RegexSet::new(&[&format!(r"^(P )?[123]{:?} X$", bid)]).unwrap(),
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
                        regex: RegexSet::new(&[
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
                    regex: RegexSet::new(&[&format!(
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
                        regex: RegexSet::new(&[
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
            regex: RegexSet::new(&[r"^P P P 1H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P P 1S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P P 1C$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P P 1D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P HS$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P 1S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P 1C$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^P P 1D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1C$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1. [PX] 1S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1. [PX] 1H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1. [PX] 1D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1[CD] P 1[DH] [PX] 1S$"]).unwrap(),
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 2{:?}", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
                forcing: Forcing::Passable,
            });
            sheets.add(Convention::Simple {
                the_name: "Limit raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 3{:?}", opening, opening)])
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 3{:?}", opening, opening)])
                    .unwrap(),
                the_description: format_as!(HTML, ""),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
                forcing: Forcing::Passable,
            });
            min_support.length[opening] = 4;
            sheets.add(Convention::Simple {
                the_name: "Inverted minor limit raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 2{:?}", opening, opening)])
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
                    regex: RegexSet::new(&[&format!(
                        "^(P )*1{:?} [PX] {}{:?}",
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
                        regex: RegexSet::new(&[&format!(
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 2N", opening)]).unwrap(),
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
                    regex: RegexSet::new(&[&format!(
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
            regex: RegexSet::new(&[r"^(P )*1C [PX] 1N$"]).unwrap(),
            min: max,
            max,
            the_description: format_as!(HTML, "Do not bid!<br/>Bid 4 card suit instead!"),
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1D [PX] 1N$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1H [PX] 1N$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*1S [PX] 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 13, 2].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: format_as!(HTML, ""),
            forcing: Forcing::Passable,
        });

        let mut min_nt = min;
        min_nt.length = [2, 2, 2, 2].into();
        let mut max_nt = max;
        max_nt.length = [5, 5, 5, 5].into();
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N$"]).unwrap(),
            max: max_nt.with_hcp(17).with_shcp(18).with_lhcp(18),
            min: min_nt.with_hcp(15).with_shcp(15).with_lhcp(15),
            the_description: format_as!(HTML, ""),
            the_name: "Opening 1NT",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2N$"]).unwrap(),
            max: max_nt.with_hcp(22).with_shcp(23).with_lhcp(23),
            min: min_nt.with_hcp(20).with_shcp(20).with_lhcp(20),
            the_description: format_as!(HTML, ""),
            the_name: "Opening 2NT",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N [PX] 2N$"]).unwrap(),
            max: max.with_hcp(10),
            min: min.with_hcp(8),
            the_description: format_as!(HTML, ""),
            the_name: "Game invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N [PX] 4N$"]).unwrap(),
            max: max.with_hcp(17),
            min: min.with_hcp(16),
            the_description: format_as!(HTML, ""),
            the_name: "Slam invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2N [PX] 3N$"]).unwrap(),
            max: max.with_hcp(10),
            min: min.with_hcp(5),
            the_description: format_as!(HTML, ""),
            the_name: "Game after 2N opening",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2|2N [PX] 3)C$"]).unwrap(),
            max,
            min,
            the_description: format_as!(HTML, "4-card major?"),
            the_name: "Stayman",
            forcing: Forcing::Forcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2C [PX] 2|2N [PX] 3C [PX] 3)S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2|2N [PX] 3)D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*(1N [PX] 2|2N [PX] 3)H$"]).unwrap(),
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
            regex: RegexSet::new(&[
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

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C$"]).unwrap(),
            max,
            min: min.with_lhcp(23),
            the_description: format_as!(HTML, "hcp≥23"),
            the_name: "Strong two",
            forcing: Forcing::GameForcing,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D$"]).unwrap(),
            max: max.with_lhcp(7),
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Weak response",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 2S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 2H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 3C$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..min
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 3D$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..min
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 2N$"]).unwrap(),
            max: max.with_lhcp(23),
            min: min.with_lhcp(24),
            the_description: format_as!(HTML, ""),
            the_name: "Game invite",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D [PX] 3N$"]).unwrap(),
            max: max.with_lhcp(27),
            min: min.with_lhcp(25),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D ..? 4N$"]).unwrap(),
            max: max.with_lhcp(30),
            min: min.with_lhcp(28),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2D ..? 5N$"]).unwrap(),
            max: max.with_lhcp(32),
            min: min.with_lhcp(31),
            the_description: format_as!(HTML, ""),
            the_name: "Strong two rebid",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2H$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2S$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 3C$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 3D$"]).unwrap(),
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
            regex: RegexSet::new(&[r"^(P )*2C [PX] 2N$"]).unwrap(),
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

        for opening in [Diamonds, Hearts, Spades].iter().cloned() {
            let mut max = max.with_hcp(10).with_lhcp(12);
            max.length[opening] = 6;
            let mut min = min.with_hcp(5);
            min.length[opening] = 6;
            min.hcp_in_suit[opening] = 4;
            sheets.add(Convention::Simple {
                regex: RegexSet::new(&[&format!(r"^(P )?(P )?2{:?}$", opening)]).unwrap(),
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
                regex: RegexSet::new(&[&format!(r"^(P )?(P )?3{:?}$", opening)]).unwrap(),
                max,
                min,
                the_description: format_as!(HTML, ""),
                the_name: "Weak three",
                forcing: Forcing::Passable,
            });
        }

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*P$"]).unwrap(),
            max: max.with_lhcp(12),
            min,
            the_description: format_as!(HTML, ""),
            the_name: "Opening pass",
            forcing: Forcing::Passable,
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1. P P$"]).unwrap(),
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 2{:?}$", opening, opening)])
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} [PX] 2N$", opening)]).unwrap(),
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
                    regex: RegexSet::new(&[&format!(
                        "^(P )*1{:?} {}{:?}$",
                        opening, bid, overcall
                    )])
                    .unwrap(),
                    max: max.with_hcp(17),
                    min: min_overcall,
                    the_description: format_as!(HTML, ""),
                    forcing: Forcing::Passable,
                });

                sheets.add(Convention::Simple {
                    the_name: "Overcall",
                    regex: RegexSet::new(&[&format!(
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
                        regex: RegexSet::new(&[&format!(
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

        for suit in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut min = min;
            min.length[suit] = 8;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new(&[&format!("[12]. [12]. .*2{:?}$", suit)]).unwrap(),
                max,
                min,
            });
            min.length[suit] = 9;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new(&[&format!("[123]. [123]. .*3{:?}$", suit)]).unwrap(),
                max,
                min,
            });
            min.length[suit] = 10;
            sheets.add(Convention::Natural {
                the_name: "Law of total tricks",
                regex: RegexSet::new(&[&format!("[123]. [123]. .*4{:?}$", suit)]).unwrap(),
                max,
                min,
            });
        }

        sheets.add(Convention::Natural {
            the_name: "Natural game",
            regex: RegexSet::new(&["3N$"]).unwrap(),
            min: min.with_hcp(26),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            the_name: "Natural",
            regex: RegexSet::new(&["6N$"]).unwrap(),
            min: min.with_hcp(33),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            the_name: "Natural",
            regex: RegexSet::new(&["7N$"]).unwrap(),
            min: min.with_hcp(37),
            max: HandValuation::MAX,
        });
        for major in [Hearts, Spades].iter().cloned() {
            let mut length = min.length;
            length[major] = 8;
            sheets.add(Convention::Natural {
                the_name: "Natural game",
                regex: RegexSet::new(&[&format!("4{:?}$", major)]).unwrap(),
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
                regex: RegexSet::new(&[&format!("5{:?}$", minor)]).unwrap(),
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
                regex: RegexSet::new(&[&format!("6{:?}$", suit)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 33,
                    length,
                    ..min
                },
            });
            sheets.add(Convention::Natural {
                the_name: "Natural",
                regex: RegexSet::new(&[&format!("7{:?}$", suit)]).unwrap(),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 37,
                    length,
                    ..min
                },
            });

            sheets.add(Convention::Natural {
                the_name: "Natural",
                regex: RegexSet::new(&[&format!("3{:?}$", suit)]).unwrap(),
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
            regex: RegexSet::new(&["1. P 1N$", "2. P 2N$"]).unwrap(),
            min: HandValuation::MIN,
            max: HandValuation::MAX,
        });
        sheets
    }
}

#[test]
fn test_bidai() {
    use crate::Suit::*;
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
fn test_sheets() {
    use crate::Suit::*;
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
