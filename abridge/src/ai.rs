use crate::{Bid, GameState};
use bridge_deck::{Card, Cards, HandValuation};
use regex::RegexSet;
pub trait BidAI: std::fmt::Debug {
    fn bid(&mut self, history: &[Bid]) -> Bid;
}
pub trait PlayAI: std::fmt::Debug {
    fn play(&mut self, game: &GameState) -> Card;
}

#[derive(Debug)]
pub struct AllPass;
impl BidAI for AllPass {
    fn bid(&mut self, _history: &[Bid]) -> Bid {
        Bid::Pass
    }
}

#[derive(Debug)]
pub struct RandomPlay;
impl PlayAI for RandomPlay {
    fn play(&mut self, game: &GameState) -> Card {
        game.could_be_played().pick(1).unwrap().next().unwrap()
    }
}

use display_as::{format_as, with_template, DisplayAs, HTML, UTF8};

#[derive(Clone)]
pub enum Convention {
    Simple {
        regex: RegexSet,
        the_description: String,
        the_name: &'static str,
        max: HandValuation,
        min: HandValuation,
    },
    Natural {
        bid: Bid,
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

fn format_range(s: &mut String, min: u8, max: u8, theoretical_max: u8, name: &str) {
    if max == min {
        s.push_str(&format!("{}", max));
    } else if min > 0 && max < theoretical_max {
        s.push_str(&format!("{}-{}", min, max));
    } else if min > 0 {
        s.push_str(&format!("{}+", min));
    } else if max < theoretical_max {
        s.push_str(&format!("<{}", max + 1));
    } else {
        return;
    }
    s.push(' ');
    s.push_str(name);
    s.push_str("<br/>")
}

impl Convention {
    fn min_valuation(&self, actual_bids: &[Bid]) -> HandValuation {
        match self.refine(actual_bids) {
            None => HandValuation::MIN,
            Some(Convention::Simple { min, .. }) => min,
            Some(Convention::Natural { min, .. }) => min, // FIXME
            _ => unreachable!(),
        }
    }
    fn max_valuation(&self, actual_bids: &[Bid]) -> HandValuation {
        match self.refine(actual_bids) {
            None => HandValuation::MIN,
            Some(Convention::Simple { max, .. }) => max,
            Some(Convention::Natural { max, .. }) => max, // FIXME
            _ => unreachable!(),
        }
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
            Convention::Natural { bid, .. } => {
                if actual_bids.last() == Some(bid) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            Convention::Ordered { conventions, .. } => {
                match conventions.iter().filter(|c| c.applies(actual_bids)).next() {
                    Some(Convention::Natural { bid, min, max }) => {
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
                        let bid = *bid;
                        match bid {
                            Bid::NT(_) => {
                                min.hcp = min.hcp.saturating_sub(std::cmp::max(partner.lhcp, partner.hcp));
                            }
                            Bid::Suit(_, suit) => {
                                min.hcp = min
                                    .hcp
                                    .saturating_sub(std::cmp::max(partner.lhcp, partner.hcp));
                                if partner_max.hcp < HandValuation::MAX.hcp {
                                    max.hcp = min.hcp + 2;
                                }
                                min.length[suit] =
                                    min.length[suit].saturating_sub(partner.length[suit]);
                                if myself.length[suit] >= min.length[suit] {
                                    min.length[suit] = 0;
                                }
                            }
                            _ => (),
                        }
                        Some(Convention::Natural { bid, min, max })
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
        use bridge_deck::Suit;
        match self {
            Convention::Simple {
                the_name,
                the_description,
                max,
                min,
                ..
            } => {
                let mut s = format!("<strong>{}</strong><br/>", the_name);
                format_range(&mut s, min.hcp, max.hcp, HandValuation::MAX.hcp, "hcp");
                format_range(&mut s, min.shcp, max.shcp, HandValuation::MAX.lhcp, "shcp");
                format_range(&mut s, min.lhcp, max.lhcp, HandValuation::MAX.lhcp, "lhcp");
                for suit in Suit::ALL.iter().cloned() {
                    format_range(
                        &mut s,
                        min.length[suit],
                        max.length[suit],
                        13,
                        &format_as!(HTML, suit),
                    );
                    format_range(
                        &mut s,
                        min.hcp_in_suit[suit],
                        max.hcp_in_suit[suit],
                        10,
                        &format_as!(HTML, "hcp in " suit),
                    );
                    format_range(
                        &mut s,
                        min.hcp_outside_suit[suit],
                        max.hcp_outside_suit[suit],
                        30,
                        &format_as!(HTML, "hcp outside" suit),
                    );
                }
                s.push_str(&the_description);
                RawHtml(s)
            }
            Convention::Natural { bid, min, .. } => {
                let mut s = format_as!(HTML, "<strong>Natural " bid "</strong><br/>");
                let max = HandValuation::MAX;
                format_range(&mut s, min.hcp, min.hcp + 2, max.hcp, "hcp");
                for suit in Suit::ALL.iter().cloned() {
                    format_range(
                        &mut s,
                        min.length[suit],
                        max.length[suit],
                        13,
                        &format_as!(HTML, suit),
                    );
                    format_range(
                        &mut s,
                        min.hcp_in_suit[suit],
                        max.hcp_in_suit[suit],
                        10,
                        &format_as!(HTML, "hcp in " suit),
                    );
                    format_range(
                        &mut s,
                        min.hcp_outside_suit[suit],
                        max.hcp_outside_suit[suit],
                        30,
                        &format_as!(HTML, "hcp outside" suit),
                    );
                }
                RawHtml(s)
            }
            Convention::Ordered { the_name, .. } => RawHtml(the_name.to_string()),
        }
    }
    /// Name of the convention
    fn name(&self) -> String {
        match self {
            Convention::Simple { the_name, .. } => the_name.to_string(),
            Convention::Natural { bid, .. } => format_as!(HTML, "Natural " bid),
            Convention::Ordered { the_name, .. } => the_name.to_string(),
        }
    }
    /// Does this bid work for this hand?
    fn _is_appropriate(&self, _bids: &[Bid], _hand: Cards) -> bool {
        todo!()
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
            Convention::Simple { .. } | Convention::Natural { .. } => {
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
        let min = HandValuation::MIN;
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
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1C$"]).unwrap(),
            the_description: format_as!(HTML, Clubs "≥" Diamonds),
            the_name: "Opening bid",
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            },
            min: HandValuation {
                lhcp: 13,
                length: [3, 0, 0, 0].into(),
                ..min
            },
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1D$"]).unwrap(),
            the_description: format_as!(HTML, Diamonds ">" Clubs),
            the_name: "Opening bid",
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            },
            min: HandValuation {
                lhcp: 13,
                length: [0, 3, 0, 0].into(),
                ..min
            },
        });

        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1S$"]).unwrap(),
            the_description: format_as!(HTML, Spades ">" Hearts),
            max,
            min: HandValuation {
                hcp: 6,
                length: [0, 0, 0, 4].into(),
                ..min
            },
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1H$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "≥" Spades),
            max,
            min: HandValuation {
                hcp: 6,
                length: [0, 0, 4, 0].into(),
                ..min
            },
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1D$"]).unwrap(),
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
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}", opening, opening)]).unwrap(),
                the_description: "".to_string(),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
            });
            sheets.add(Convention::Simple {
                the_name: "Limit raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 3{:?}", opening, opening)]).unwrap(),
                the_description: "".to_string(),
                max: max.with_hcp(12),
                min: min_support.with_hcp(11),
            });
        }
        for opening in [Clubs, Diamonds].iter().cloned() {
            let mut min_support = min;
            min_support.length[opening] = 4;
            let max = HandValuation {
                length: [13, 13, 4, 4].into(),
                ..max
            };
            sheets.add(Convention::Simple {
                the_name: "Inverted minor weak raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 3{:?}", opening, opening)]).unwrap(),
                the_description: "".to_string(),
                max: max.with_hcp(10),
                min: min_support.with_hcp(6),
            });
            sheets.add(Convention::Simple {
                the_name: "Inverted minor strong raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}", opening, opening)]).unwrap(),
                the_description: "".to_string(),
                max,
                min: min_support.with_hcp(11),
            });
        }
        for opening in [Hearts, Spades].iter().cloned() {
            let mut min_splinter = min;
            min_splinter.length[opening] = 4;
            min_splinter.hcp_outside_suit[opening] = 10;
            for response in [Clubs, Diamonds, Hearts, Spades]
                .iter()
                .cloned()
                .filter(|s| *s != opening)
            {
                let mut max_splinter = max.with_hcp(12);
                max_splinter.length[response] = 1;
                max_splinter.hcp_in_suit[response] = 1;
                max_splinter.hcp_outside_suit[response] = 12;
                let splinterbid = if response > opening { 3 } else { 4 };
                sheets.add(Convention::Simple {
                    the_name: "Splinter",
                    regex: RegexSet::new(&[&format!(
                        "^(P )*1{:?} P {}{:?}",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    the_description: "".to_string(),
                    max: max_splinter,
                    min: min_splinter,
                });
                if response < opening {
                    let mut min = min.with_hcp(11);
                    min.length[response] = 4;
                    sheets.add(Convention::Simple {
                        the_name: "2/1 response",
                        regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}$", opening, response)])
                            .unwrap(),
                        the_description: "".to_string(),
                        max,
                        min,
                    });
                }
            }
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
                        "^(P )*1{:?} P {}{:?}$",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    max: max_splinter,
                    min: min_splinter,
                    the_description: "".to_string(),
                });
            }
        }

        sheets.add(Convention::Simple {
            the_name: "Impossible response",
            regex: RegexSet::new(&[r"^(P )*1C P 1N$"]).unwrap(),
            max,
            min,
            the_description: format_as!(HTML, "Do not bid!<br/>Bid 4 card suit instead!"),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1D P 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 4, 3, 3].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: "".to_string(),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1H P 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 2, 3].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: "".to_string(),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1S P 1N$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 13, 2].into(),
                hcp: 10,
                ..max
            },
            min: min.with_hcp(6),
            the_description: "".to_string(),
        });

        let mut min_nt = min;
        min_nt.length = [2, 2, 2, 2].into();
        let mut max_nt = max;
        max_nt.length = [5, 5, 5, 5].into();
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N$"]).unwrap(),
            max: max_nt.with_hcp(17).with_shcp(18).with_lhcp(18),
            min: min_nt.with_hcp(15).with_shcp(15).with_lhcp(15),
            the_description: "".to_string(),
            the_name: "Opening 1NT",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2N$"]).unwrap(),
            max: max_nt.with_hcp(22).with_shcp(23).with_lhcp(23),
            min: min_nt.with_hcp(20).with_shcp(20).with_lhcp(20),
            the_description: "".to_string(),
            the_name: "Opening 2NT",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N P 2N$"]).unwrap(),
            max: max.with_hcp(10),
            min: min.with_hcp(8),
            the_description: "".to_string(),
            the_name: "Game invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N P 4N$"]).unwrap(),
            max: max.with_hcp(17),
            min: min.with_hcp(16),
            the_description: "".to_string(),
            the_name: "Slam invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)C$"]).unwrap(),
            max,
            min,
            the_description: format_as!(HTML, "4-card major?"),
            the_name: "Stayman",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)D$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 3, 3].into(),
                ..max
            },
            min,
            the_description: "".to_string(),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 4, 0].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)S$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 4].into(),
                ..min
            },
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)D$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Jacobi transfer",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Jacobi transfer",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C$"]).unwrap(),
            max,
            min: min.with_lhcp(23),
            the_description: format_as!(HTML, "hcp≥23"),
            the_name: "Strong two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D$"]).unwrap(),
            max: max.with_lhcp(7),
            min,
            the_description: "".to_string(),
            the_name: "Weak response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2S$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                ..min
            },
            the_description: format_as!(HTML, Spades ">" Hearts),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3C$"]).unwrap(),
            max: HandValuation {
                length: [13, 13, 4, 4].into(),
                ..min
            },
            min: HandValuation {
                length: [5, 0, 0, 0].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3D$"]).unwrap(),
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
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2N$"]).unwrap(),
            max: max.with_lhcp(23),
            min: min.with_lhcp(24),
            the_description: "".to_string(),
            the_name: "Game invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3N$"]).unwrap(),
            max: max.with_lhcp(27),
            min: min.with_lhcp(25),
            the_description: "".to_string(),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D ..? 4N$"]).unwrap(),
            max: max.with_lhcp(30),
            min: min.with_lhcp(28),
            the_description: "".to_string(),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D ..? 5N$"]).unwrap(),
            max: max.with_lhcp(32),
            min: min.with_lhcp(31),
            the_description: "".to_string(),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2H$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 5, 0].into(),
                hcp_in_suit: [0, 0, 3, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2S$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 0, 0, 5].into(),
                hcp_in_suit: [0, 0, 0, 3].into(),
                hcp: 8,
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 3C$"]).unwrap(),
            max,
            min: HandValuation {
                length: [5, 0, 0, 0].into(),
                hcp_in_suit: [3, 0, 0, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 3D$"]).unwrap(),
            max,
            min: HandValuation {
                length: [0, 5, 0, 0].into(),
                hcp_in_suit: [0, 3, 0, 0].into(),
                hcp: 8,
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2N$"]).unwrap(),
            max: HandValuation {
                length: [5, 5, 5, 5].into(),
                ..max
            },
            min: HandValuation {
                hcp: 8,
                length: [2, 2, 2, 2].into(),
                ..min
            },
            the_description: "".to_string(),
            the_name: "Strong response",
        });

        for opening in [Diamonds, Hearts, Spades].iter().cloned() {
            let mut max = max.with_hcp(10);
            max.length[opening] = 6;
            let mut min = min.with_hcp(5);
            min.length[opening] = 6;
            min.hcp_in_suit[opening] = 4;
            sheets.add(Convention::Simple {
                regex: RegexSet::new(&[&format!(r"^(P )?(P )?2{:?}$", opening)]).unwrap(),
                max,
                min,
                the_description: "".to_string(),
                the_name: "Weak two",
            });
        }
        for opening in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut max = max.with_hcp(9);
            max.length[opening] = 7;
            let mut min = min.with_hcp(5);
            min.length[opening] = 7;
            min.hcp_in_suit[opening] = 4;
            sheets.add(Convention::Simple {
                regex: RegexSet::new(&[&format!(r"^(P )?(P )?3{:?}$", opening)]).unwrap(),
                max,
                min,
                the_description: "".to_string(),
                the_name: "Weak three",
            });
        }

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*P$"]).unwrap(),
            max: max.with_lhcp(12),
            min,
            the_description: "".to_string(),
            the_name: "Opening pass",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1. P P$"]).unwrap(),
            max: max.with_hcp(5),
            min,
            the_description: "".to_string(),
            the_name: "Response pass",
        });

        sheets.add(Convention::Natural {
            bid: Bid::NT(3),
            min: min.with_hcp(26),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            bid: Bid::NT(6),
            min: min.with_hcp(33),
            max: HandValuation::MAX,
        });
        sheets.add(Convention::Natural {
            bid: Bid::NT(7),
            min: min.with_hcp(37),
            max: HandValuation::MAX,
        });
        for major in [Hearts, Spades].iter().cloned() {
            let mut length = min.length;
            length[major] = 8;
            sheets.add(Convention::Natural {
                bid: Bid::Suit(4, major),
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
                bid: Bid::Suit(5, minor),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 28,
                    length,
                    ..min
                },
            });
        }
        for suit in [Clubs, Diamonds, Hearts, Spades].iter().cloned() {
            let mut length = min.length;
            length[suit] = 8;
            sheets.add(Convention::Natural {
                bid: Bid::Suit(6, suit),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 33,
                    length,
                    ..min
                },
            });
            sheets.add(Convention::Natural {
                bid: Bid::Suit(7, suit),
                max: HandValuation::MAX,
                min: HandValuation {
                    hcp: 37,
                    length,
                    ..min
                },
            });
        }
        sheets
    }
}

#[test]
fn test_sheets() {
    use crate::Suit::*;
    use Bid::*;
    let sheets = Convention::sheets();
    assert_eq!(None, sheets.refine(&[]).map(|c| c.name()));
    assert_eq!(
        Some("Opening pass".to_string()),
        sheets.refine(&[Pass]).map(|c| c.name())
    );
    assert_eq!(
        Some("Opening bid".to_string()),
        sheets.refine(&[Suit(1, Clubs)]).map(|c| c.name())
    );
    assert_eq!(
        None,
        sheets
            .refine(&[Suit(1, Clubs), Suit(1, Clubs)])
            .map(|c| c.name())
    );
}

struct RawHtml(String);
#[with_template(self.0 as UTF8)]
impl DisplayAs<HTML> for RawHtml {}
