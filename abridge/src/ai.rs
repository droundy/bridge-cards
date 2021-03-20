use crate::{Bid, GameState};
use bridge_deck::{Card, Cards};
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

impl Convention {
    pub fn refine(&self, actual_bids: &[Bid]) -> Option<&Convention> {
        match self {
            Convention::Simple { regex, .. } => {
                if regex.is_match(&bids_string(actual_bids)) {
                    Some(self)
                } else {
                    None
                }
            }
            Convention::Ordered { conventions, .. } => {
                conventions.iter().filter(|c| c.applies(actual_bids)).next()
            }
        }
    }
    pub fn refine2(&self, bid: Bid, otherbids: &[Bid]) -> Option<&Convention> {
        let mut bids = otherbids.iter().cloned().collect::<Vec<_>>();
        bids.push(bid);
        self.refine(&bids)
    }
    /// Does this convention apply to the last bid in this sequence?
    pub fn applies(&self, actual_bids: &[Bid]) -> bool {
        self.refine(actual_bids).is_some()
    }
    /// A short description of the meaning of this bid.
    fn description(&self) -> String {
        match self {
            Convention::Simple {
                the_name,
                the_description,
                ..
            } => format!("<strong>{}</strong><br/>{}", the_name, the_description),
            Convention::Ordered { the_name, .. } => the_name.to_string(),
        }
    }
    /// Name of the convention
    fn name(&self) -> String {
        match self {
            Convention::Simple { the_name, .. } => the_name.to_string(),
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
            Convention::Simple { .. } => {
                let s = self.clone();
                *self = Convention::Ordered {
                    conventions: vec![s, convention],
                    the_name: "",
                }
            }
        }
    }
}

#[with_template(self.description() as UTF8)]
impl DisplayAs<HTML> for Convention {}

impl Convention {
    pub fn sheets() -> Self {
        let mut sheets = Convention::new("Sheets");
        use bridge_deck::Suit::*;
        sheets.add(Convention::Simple {
            the_name: "Opening bid",
            regex: RegexSet::new(&[r"^(P )*1H$"]).unwrap(),
            the_description: format_as!(HTML, "lhcp≥13<br/>" Hearts "≥5<br/>" Hearts "≥" Spades),
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1S$"]).unwrap(),
            the_description: format_as!(HTML, "lhcp≥13<br/>" Spades "≥5<br/>" Spades ">" Hearts),
            the_name: "Opening bid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1C$"]).unwrap(),
            the_description: format_as!(HTML, "lhcp≥13<br/>" Clubs "≥3<br/>" Clubs "≥" Diamonds "<br/>" Hearts "<5<br/>" Spades "<5"),
            the_name: "Opening bid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1D$"]).unwrap(),
            the_description: format_as!(HTML, "lhcp≥13<br/>" Diamonds "≥3<br/>" Diamonds ">♣<br/>" Hearts "<5<br/>♠<5"),
            the_name: "Opening bid",
        });

        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1S$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥6<br/>" Spades "≥4<br/>" Spades ">" Hearts),
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1H$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥6<br/>" Hearts "≥4<br/>" Hearts "≥" Spades),
        });
        sheets.add(Convention::Simple {
            the_name: "Response",
            regex: RegexSet::new(&[r"^(P )*1. P 1D$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥6<br/>" Diamonds "≥4<br/>"
                                        Diamonds ">" Hearts "<br/>" Hearts "<4<br/>" Spades "<4"),
        });
        for response in [Clubs, Diamonds].iter() {
            sheets.add(Convention::Simple {
                the_name: "2/1 response",
                regex: RegexSet::new(&[&format!("^(P )*1H P 2{:?}", response)]).unwrap(),
                the_description: format_as!(HTML, "hcp≥13<br/>" response "≥4<br/>" Spades "<4"),
            });
        }

        for opening in [Hearts, Spades].iter() {
            sheets.add(Convention::Simple {
                the_name: "Single raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}", opening, opening)]).unwrap(),
                the_description: format_as!(HTML, "6-10 hcp<br/>" opening "≥3"),
            });
            sheets.add(Convention::Simple {
                the_name: "Limit raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 3{:?}", opening, opening)]).unwrap(),
                the_description: format_as!(HTML, "11-12 hcp<br/>" opening "≥3"),
            });
        }
        for opening in [Clubs, Diamonds].iter() {
            sheets.add(Convention::Simple {
                the_name: "Inverted minor weak raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 3{:?}", opening, opening)]).unwrap(),
                the_description: format_as!(HTML, "6-10 hcp<br/>" opening "≥4<br/>" Hearts "<4<br/>" Spades "<4"),
            });
            sheets.add(Convention::Simple {
                the_name: "Inverted minor strong raise",
                regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}", opening, opening)]).unwrap(),
                the_description: format_as!(HTML, "11+ hcp<br/>" opening "≥4<br/>" Hearts "<4<br/>" Spades "<4"),
            });
        }
        for opening in [Hearts, Spades].iter() {
            for response in [Clubs, Diamonds, Hearts, Spades]
                .iter()
                .filter(|s| *s != opening)
            {
                let splinterbid = if response > opening { 3 } else { 4 };
                sheets.add(Convention::Simple {
                    the_name: "Splinter",
                    regex: RegexSet::new(&[&format!(
                        "^(P )*1{:?} P {}{:?}",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    the_description: format_as!(HTML, "10-12 hcp<br/>" response "≤1<br/>"
                                                opening "≥4"),
                });
                if response < opening {
                    sheets.add(Convention::Simple {
                        the_name: "2/1 response",
                        regex: RegexSet::new(&[&format!("^(P )*1{:?} P 2{:?}", opening, response)])
                            .unwrap(),
                        the_description: format_as!(HTML, "hcp≥13<br/>" response "≥4"),
                    });
                }
            }
        }
        for opening in [Clubs, Diamonds].iter() {
            for response in [Diamonds, Hearts, Spades].iter().filter(|s| *s != opening) {
                let splinterbid = if response > opening { 3 } else { 4 };
                sheets.add(Convention::Simple {
                    the_name: "Splinter",
                    regex: RegexSet::new(&[&format!(
                        "^(P )*1{:?} P {}{:?}",
                        opening, splinterbid, response
                    )])
                    .unwrap(),
                    the_description: format_as!(HTML, "10-12 hcp<br/>" response "≤1<br/>"
                                                opening "≥5<br/>" Hearts "<4<br/>" Spades "<4"),
                });
            }
        }

        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1C P 1N$"]).unwrap(),
            the_description: format_as!(HTML, "Do not bid!<br/>Bid 4 card suit instead!"),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1D P 1N$"]).unwrap(),
            the_description: format_as!(HTML, "6-10 hcp<br/>" Diamonds "<5<br/>" Hearts "<4<br/>" Spades "<4"),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1H P 1N$"]).unwrap(),
            the_description: format_as!(HTML, "6-10 hcp<br/>" Hearts "<3<br/>" Spades "<4"),
        });
        sheets.add(Convention::Simple {
            the_name: "Weak response",
            regex: RegexSet::new(&[r"^(P )*1S P 1N$"]).unwrap(),
            the_description: format_as!(HTML, "6-10 hcp<br/>" Spades "<3"),
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N$"]).unwrap(),
            the_description: format_as!(HTML, "15-17 hcp<br/>15-18 shcp<br/>15-18 lhcp"),
            the_name: "Opening 1NT",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2N$"]).unwrap(),
            the_description: format_as!(HTML, "20-22 hcp<br/>20-23 shcp<br/>20-23 lhcp"),
            the_name: "Opening 2NT",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N P 2N$"]).unwrap(),
            the_description: format_as!(HTML, "8-10 hcp"),
            the_name: "Game invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1N P 4N$"]).unwrap(),
            the_description: format_as!(HTML, "16-17 hcp"),
            the_name: "Slam invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)C$"]).unwrap(),
            the_description: format_as!(HTML, "4-card major?"),
            the_name: "Stayman",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)D$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "<4<br/>" Spades "<4"),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)H$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "≥4<br/>"),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2C P 2|2N P 3C P 3)S$"]).unwrap(),
            the_description: format_as!(HTML, Spades "≥4<br/>" Spades ">" Hearts),
            the_name: "Stayman response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)D$"]).unwrap(),
            the_description: format_as!(HTML, "" Hearts "≥5"),
            the_name: "Jacobi transfer",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*(1N P 2|2N P 3)H$"]).unwrap(),
            the_description: format_as!(HTML, "" Spades "≥5"),
            the_name: "Jacobi transfer",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥23"),
            the_name: "Strong two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D$"]).unwrap(),
            the_description: format_as!(HTML, "0-7 hcp"),
            the_name: "Weak response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2S$"]).unwrap(),
            the_description: format_as!(HTML, Spades "≥5<br/>" Spades ">" Hearts),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2H$"]).unwrap(),
            the_description: format_as!(HTML, Hearts "≥5"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3C$"]).unwrap(),
            the_description: format_as!(HTML, Clubs "≥5<br/>" Spades "<5<br/>" Hearts "<5"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3D$"]).unwrap(),
            the_description: format_as!(HTML, Diamonds "≥5<br/>" Diamonds ">" Clubs "<br/>" Spades "<5<br/>" Hearts "<5"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 2N$"]).unwrap(),
            the_description: format_as!(HTML, "23-24 hcp"),
            the_name: "Game invite",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D P 3N$"]).unwrap(),
            the_description: format_as!(HTML, "25-27 hcp"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D ..? 4N$"]).unwrap(),
            the_description: format_as!(HTML, "28-30 hcp"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2D ..? 5N$"]).unwrap(),
            the_description: format_as!(HTML, "31-32 hcp"),
            the_name: "Strong two rebid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2H$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥8<br/>" Hearts "≥5"),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2S$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥8<br/>" Spades "≥5"),
            the_name: "Strong response",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*2C P 2N$"]).unwrap(),
            the_description: format_as!(HTML, "hcp≥8<br/>" Hearts "≲5" Spades "≲5"),
            the_name: "Strong response",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?2H$"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp<br/>6" Hearts),
            the_name: "Weak two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?2S$"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp<br/>6♠"),
            the_name: "Weak two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?2D$"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp<br/>6" Diamonds),
            the_name: "Weak two",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?3H$"]).unwrap(),
            the_description: format_as!(HTML, "5-9 hcp<br/>7" Hearts),
            the_name: "Weak three",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?3S$"]).unwrap(),
            the_description: format_as!(HTML, "5-9 hcp<br/>7" Spades),
            the_name: "Weak three",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?3D$"]).unwrap(),
            the_description: format_as!(HTML, "5-9 hcp<br/>7" Diamonds),
            the_name: "Weak three",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )?(P )?3C$"]).unwrap(),
            the_description: format_as!(HTML, "5-9 hcp<br/>7" Clubs),
            the_name: "Weak three",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*P$"]).unwrap(),
            the_description: "lhcp<13".to_string(),
            the_name: "Opening pass",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"^(P )*1. P P$"]).unwrap(),
            the_description: "hcp<6".to_string(),
            the_name: "Response pass",
        });
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
