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
                write!(&mut s, "{}{} ", n, suit.unicode()).unwrap();
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
                the_description, ..
            } => the_description.clone(),
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
            regex: RegexSet::new(&[r"(P )*1♥"]).unwrap(),
            the_description: format_as!(HTML, "13+ lhcp, " Hearts "≥5, " Hearts "≥" Spades),
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )*1♠"]).unwrap(),
            the_description: format_as!(HTML, "13+ lhcp, " Spades "≥5, " Spades ">" Hearts),
            the_name: "Opening bid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )*1♣"]).unwrap(),
            the_description: format_as!(HTML, "13+ lhcp, ♣≥3, ♣≥" Diamonds " " Hearts "<5, ♠≥5"),
            the_name: "Opening bid",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )*1♦"]).unwrap(),
            the_description: format_as!(HTML, "13+ lhcp, " Diamonds "≥3, " Diamonds ">♣, " Hearts "<5, ♠≥5"),
            the_name: "Opening bid",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )?(P )?2♠"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp, 6" Hearts),
            the_name: "Weak two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )?(P )?2♠"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp, 6♠"),
            the_name: "Weak two",
        });
        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )?(P )?2♠"]).unwrap(),
            the_description: format_as!(HTML, "5-10 hcp, 6" Diamonds),
            the_name: "Weak two",
        });

        sheets.add(Convention::Simple {
            regex: RegexSet::new(&[r"(P )*P"]).unwrap(),
            the_description: "Less than 13 lhcp".to_string(),
            the_name: "Opening pass",
        });
        sheets
    }
}
