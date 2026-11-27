//! Which auctions a table entry describes, and how to tell.
//!
//! An entry's auctions are written in the notation that the tables have
//! always used, which looks like a regular expression over `bids_string`: the
//! calls of the auction separated by spaces, such as `^(P )*1N [PX] 2C$`.
//! [`Auctions::new`] reads that notation, with the little of it that the tables
//! use, into [`Pattern`]s that are matched against a slice of [`Bid`]s with no
//! text in between:
//!
//! - `^` at the start: the pattern starts at the start of the auction.
//!   Without it, the pattern matches the end of any auction.  After the `^`,
//!   `(P )*` is any number of passes, and each `(P )?` allows one more.
//! - `$` at the end, as the pattern must end with the last call.
//! - A call: `P`, `X`, `XX`, or a contract such as `1N` or `2C`.  In place of
//!   its characters may be a `.` for any character, a class such as `[PX]`,
//!   `[CDHSN]`, `[123]` or `[1-7]`, and a `?` after a character to make it
//!   optional, so that `..?` is any call.
//! - `.*` before the last call of a pattern: any number of calls.
//! - `(a|b)` groups of alternatives, which become several patterns, and `(a)?`
//!   groups, which make a pattern with and without them.

use crate::Bid;

/// A set of calls, with a bit for each at its `Bid::index`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallSet(u64);

impl CallSet {
    /// The calls in the set, from lowest to highest.
    fn members(self) -> impl Iterator<Item = Bid> {
        (0..Bid::CALLS)
            .filter(move |i| self.0 & (1 << i) != 0)
            .map(Bid::from_index)
    }
    /// Is `bid` one of the calls?
    pub fn contains(self, bid: Bid) -> bool {
        self.0 & (1 << bid.index()) != 0
    }
    /// The call that this set is the only member of, if it is.
    fn only(self) -> Option<Bid> {
        let mut members = self.members();
        let only = members.next()?;
        members.next().is_none().then_some(only)
    }
}

/// Every call as `bids_string` writes it, by place in `CallSet`.  Reading
/// notation asks about every call for every call it names, so these are
/// written once.
fn call_texts() -> &'static [Vec<char>] {
    static TEXTS: std::sync::OnceLock<Vec<Vec<char>>> = std::sync::OnceLock::new();
    TEXTS.get_or_init(|| {
        (0..Bid::CALLS)
            .map(|i| text(Bid::from_index(i)).chars().collect())
            .collect()
    })
}

/// A call as `bids_string` writes it.
fn text(bid: Bid) -> String {
    match bid {
        Bid::Pass => "P".to_string(),
        Bid::Double => "X".to_string(),
        Bid::Redouble => "XX".to_string(),
        Bid::Suit(level, suit) => format!("{}{}", level, suit.latin()),
        Bid::NT(level) => format!("{}N", level),
    }
}

/// What comes before the first call that a pattern names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Lead {
    /// Nothing but passes, and no more than this many of them if there is a
    /// limit.
    Passes(Option<usize>),
    /// Whatever.
    Anywhere,
}

/// A part of a pattern, which matches calls.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Step {
    /// Any number of calls.
    Gap,
    /// One call.
    Call(CallSet),
}

/// A kind of auction: the calls at the end of it, and what comes before.
#[derive(Clone, Debug)]
pub struct Pattern {
    lead: Lead,
    steps: Vec<Step>,
    /// The notation that this was read from, to show.
    source: String,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Pattern) -> bool {
        self.lead == other.lead && self.steps == other.steps
    }
}

impl Pattern {
    /// Does this pattern describe `bids`?
    pub fn matches(&self, bids: &[Bid]) -> bool {
        self.matches_steps(&self.steps, bids)
    }
    /// Do `steps` match the end of `bids`, and the lead what is before them?
    fn matches_steps(&self, steps: &[Step], bids: &[Bid]) -> bool {
        match steps.split_last() {
            None => match self.lead {
                Lead::Anywhere => true,
                Lead::Passes(limit) => {
                    limit.is_none_or(|limit| bids.len() <= limit)
                        && bids.iter().all(|&b| b == Bid::Pass)
                }
            },
            Some((&Step::Call(calls), steps)) => match bids.split_last() {
                Some((&bid, bids)) => calls.contains(bid) && self.matches_steps(steps, bids),
                None => false,
            },
            Some((&Step::Gap, steps)) => {
                (0..=bids.len()).any(|n| self.matches_steps(steps, &bids[..bids.len() - n]))
            }
        }
    }
    /// The bid that this pattern ends with, if it is always one bid.
    pub fn last_bid(&self) -> Option<Bid> {
        match self.steps.last() {
            Some(&Step::Call(calls)) => calls.only(),
            _ => None,
        }
    }
    /// The notation that this pattern was read from.
    pub fn source(&self) -> &str {
        &self.source
    }
}

/// The auctions that a table entry describes: those that match any of its
/// patterns.
#[derive(Clone, Debug, Default)]
pub struct Auctions(Vec<Pattern>);

impl Auctions {
    /// The auctions that match any of `notations`, or what is wrong with one
    /// of them.
    pub fn new<S: AsRef<str>>(notations: impl IntoIterator<Item = S>) -> Result<Auctions, String> {
        let mut patterns: Vec<Pattern> = Vec::new();
        for notation in notations {
            let notation = notation.as_ref();
            for pattern in read(notation).map_err(|e| format!("{e} in {notation:?}"))? {
                if !patterns.contains(&pattern) {
                    patterns.push(pattern);
                }
            }
        }
        Ok(Auctions(patterns))
    }
    /// No auctions at all.
    pub fn empty() -> Auctions {
        Auctions(Vec::new())
    }
    /// The auctions that match `pattern` alone.
    pub fn of(pattern: Pattern) -> Auctions {
        Auctions(vec![pattern])
    }
    /// Does one of the patterns describe `bids`?
    pub fn matches(&self, bids: &[Bid]) -> bool {
        self.0.iter().any(|pattern| pattern.matches(bids))
    }
    /// The patterns, with a group of alternatives written out as one for each.
    pub fn patterns(&self) -> &[Pattern] {
        &self.0
    }
}

/// A character that a call can have at some place.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Atom {
    Any,
    One(char),
    OneOf(Vec<char>),
}

impl Atom {
    fn matches(&self, c: char) -> bool {
        match self {
            Atom::Any => true,
            Atom::One(one) => *one == c,
            Atom::OneOf(chars) => chars.contains(&c),
        }
    }
}

/// How often a group may occur.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Times {
    Once,
    Maybe,
    Any,
}

/// The notation, read.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Node {
    /// A character, which may be left out.
    Char(Atom, bool),
    /// Any number of calls, or characters.
    Gap,
    Space,
    Group(Vec<Vec<Node>>, Times),
}

struct Parser {
    chars: Vec<char>,
    at: usize,
}

impl Parser {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.at).copied()
    }
    /// Alternatives, up to the end or a closing parenthesis.
    fn alternatives(&mut self) -> Result<Vec<Vec<Node>>, String> {
        let mut alternatives = vec![self.sequence()?];
        while self.peek() == Some('|') {
            self.at += 1;
            alternatives.push(self.sequence()?);
        }
        Ok(alternatives)
    }
    fn sequence(&mut self) -> Result<Vec<Node>, String> {
        let mut nodes = Vec::new();
        while let Some(c) = self.peek() {
            if c == '|' || c == ')' {
                break;
            }
            self.at += 1;
            let atom = match c {
                '(' => {
                    let alternatives = self.alternatives()?;
                    if self.peek() != Some(')') {
                        return Err("an unclosed (".to_string());
                    }
                    self.at += 1;
                    let times = match self.peek() {
                        Some('*') => Times::Any,
                        Some('?') => Times::Maybe,
                        _ => Times::Once,
                    };
                    if times != Times::Once {
                        self.at += 1;
                    }
                    nodes.push(Node::Group(alternatives, times));
                    continue;
                }
                ' ' => {
                    nodes.push(Node::Space);
                    continue;
                }
                '.' => Atom::Any,
                '[' => self.class()?,
                '*' | '+' | '?' | '{' | '}' | '\\' | '^' | '$' | ']' => {
                    return Err(format!("unsupported {c:?}"))
                }
                c => Atom::One(c),
            };
            match self.peek() {
                Some('*') if atom == Atom::Any => {
                    self.at += 1;
                    nodes.push(Node::Gap);
                }
                Some('*') => return Err("a * after something but .".to_string()),
                Some('?') => {
                    self.at += 1;
                    nodes.push(Node::Char(atom, true));
                }
                _ => nodes.push(Node::Char(atom, false)),
            }
        }
        Ok(nodes)
    }
    /// A class of characters, after its `[`.
    fn class(&mut self) -> Result<Atom, String> {
        let mut chars = Vec::new();
        loop {
            let c = self.peek().ok_or("an unclosed [")?;
            self.at += 1;
            if c == ']' {
                return Ok(Atom::OneOf(chars));
            }
            if self.peek() == Some('-') && self.chars.get(self.at + 1).is_some_and(|&c| c != ']') {
                let to = self.chars[self.at + 1];
                self.at += 2;
                chars.extend(c..=to);
            } else {
                chars.push(c);
            }
        }
    }
}

/// A part of a pattern with its alternatives written out.
#[derive(Clone, Debug)]
enum Elem {
    Char(Atom, bool),
    Gap,
    Space,
}

/// Every way to write out `nodes`, a group of alternatives for each.
fn expand(nodes: &[Node]) -> Result<Vec<Vec<Elem>>, String> {
    let mut written = vec![Vec::new()];
    for node in nodes {
        let options: Vec<Vec<Elem>> = match node {
            Node::Char(atom, optional) => vec![vec![Elem::Char(atom.clone(), *optional)]],
            Node::Gap => vec![vec![Elem::Gap]],
            Node::Space => vec![vec![Elem::Space]],
            Node::Group(alternatives, times) => {
                if *times == Times::Any {
                    return Err("a (..)* where it is not the start".to_string());
                }
                let mut options = Vec::new();
                for alternative in alternatives {
                    options.extend(expand(alternative)?);
                }
                if *times == Times::Maybe {
                    options.push(Vec::new());
                }
                options
            }
        };
        if let [option] = options.as_slice() {
            // The usual case, with nothing to choose between.
            for before in &mut written {
                before.extend(option.iter().cloned());
            }
            continue;
        }
        let mut longer = Vec::new();
        for before in &written {
            for option in &options {
                let mut together = before.clone();
                together.extend(option.iter().cloned());
                longer.push(together);
            }
        }
        written = longer;
    }
    Ok(written)
}

/// Does a call written as `text` match these characters, which are
/// optionally there?
fn call_matches(atoms: &[(Atom, bool)], text: &[char]) -> bool {
    match atoms.split_first() {
        None => text.is_empty(),
        Some(((atom, optional), atoms)) => {
            (!text.is_empty() && atom.matches(text[0]) && call_matches(atoms, &text[1..]))
                || (*optional && call_matches(atoms, text))
        }
    }
}

/// The steps for one call of the notation, with the gap that may come
/// before it.
fn steps_of(token: &[Elem]) -> Result<Vec<Step>, String> {
    let mut steps = Vec::new();
    let mut atoms = Vec::new();
    for (i, elem) in token.iter().enumerate() {
        match elem {
            Elem::Char(atom, optional) => atoms.push((atom.clone(), *optional)),
            Elem::Gap if i == 0 => steps.push(Step::Gap),
            Elem::Gap => return Err("a .* inside a call".to_string()),
            Elem::Space => unreachable!(),
        }
    }
    if atoms.is_empty() {
        // Only a gap.
        return if steps.is_empty() {
            Err("an empty call".to_string())
        } else {
            Ok(steps)
        };
    }
    let mut calls = 0;
    for (i, call) in call_texts().iter().enumerate() {
        if call_matches(&atoms, call) {
            calls |= 1 << i;
        }
    }
    if calls == 0 {
        return Err("a call that is no bid".to_string());
    }
    steps.push(Step::Call(CallSet(calls)));
    Ok(steps)
}

/// The patterns that `notation` stands for.
fn read(notation: &str) -> Result<Vec<Pattern>, String> {
    let mut chars: Vec<char> = notation.chars().collect();
    if chars.last() != Some(&'$') {
        return Err("no $ at the end".to_string());
    }
    chars.pop();
    let anchored = chars.first() == Some(&'^');
    if anchored {
        chars.remove(0);
    }
    let mut parser = Parser { chars, at: 0 };
    let mut alternatives = parser.alternatives()?;
    if parser.at != parser.chars.len() {
        return Err("a ) with no (".to_string());
    }
    if alternatives.len() != 1 {
        return Err("alternatives that are not in a group".to_string());
    }
    let mut nodes = alternatives.pop().unwrap();
    let mut lead = Lead::Anywhere;
    if anchored {
        // Any passes before the first call.
        lead = Lead::Passes(Some(0));
        while let Some(Node::Group(alternatives, times @ (Times::Any | Times::Maybe))) =
            nodes.first()
        {
            let pass = [Node::Char(Atom::One('P'), false), Node::Space];
            if alternatives.len() != 1 || alternatives[0] != pass {
                break;
            }
            lead = match (lead, times) {
                (Lead::Passes(Some(n)), Times::Maybe) => Lead::Passes(Some(n + 1)),
                _ => Lead::Passes(None),
            };
            nodes.remove(0);
        }
    }
    let mut patterns = Vec::new();
    for elems in expand(&nodes)? {
        let mut steps = Vec::new();
        for token in elems.split(|elem| matches!(elem, Elem::Space)) {
            steps.extend(steps_of(token)?);
        }
        if !matches!(steps.last(), Some(Step::Call(_))) {
            return Err("no call at the end".to_string());
        }
        let pattern = Pattern {
            lead,
            steps,
            source: notation.to_string(),
        };
        if !patterns.contains(&pattern) {
            patterns.push(pattern);
        }
    }
    Ok(patterns)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai::all_bids;
    use bridge_deck::Suit::*;
    use Bid::*;

    fn auctions(notation: &str) -> Auctions {
        Auctions::new([notation]).unwrap()
    }

    fn matches(notation: &str, bids: &[Bid]) -> bool {
        auctions(notation).matches(bids)
    }

    #[test]
    fn calls_are_written_as_bids_string_writes_them() {
        for bid in all_bids() {
            assert_eq!(crate::ai::bids_string(&[bid]), text(bid));
        }
    }

    #[test]
    fn passes_before_the_first_call() {
        let any = auctions("^(P )*1N$");
        assert!(any.matches(&[NT(1)]));
        assert!(any.matches(&[Pass, Pass, Pass, NT(1)]));
        assert!(!any.matches(&[Suit(1, Clubs), NT(1)]));
        assert!(!any.matches(&[Pass, Suit(1, Clubs), Pass, NT(1)]));
        let few = auctions("^(P )?(P )?2H$");
        assert!(few.matches(&[Suit(2, Hearts)]));
        assert!(few.matches(&[Pass, Pass, Suit(2, Hearts)]));
        assert!(!few.matches(&[Pass, Pass, Pass, Suit(2, Hearts)]));
        let exactly = auctions("^P P P 1H$");
        assert!(exactly.matches(&[Pass, Pass, Pass, Suit(1, Hearts)]));
        assert!(!exactly.matches(&[Pass, Pass, Suit(1, Hearts)]));
        assert!(!exactly.matches(&[Pass, Pass, Pass, Pass, Suit(1, Hearts)]));
    }

    #[test]
    fn calls_and_classes() {
        let stayman = "^(P )*1N [PX] 2C$";
        assert!(matches(stayman, &[NT(1), Pass, Suit(2, Clubs)]));
        assert!(matches(stayman, &[Pass, NT(1), Double, Suit(2, Clubs)]));
        assert!(!matches(
            stayman,
            &[NT(1), Suit(2, Diamonds), Suit(2, Clubs)]
        ));
        assert!(!matches(stayman, &[NT(1), Redouble, Suit(2, Clubs)]));
        // A `.` is any character, including the N of notrump.
        let level_one = "^(P )*1. P P$";
        assert!(matches(level_one, &[NT(1), Pass, Pass]));
        assert!(matches(level_one, &[Suit(1, Hearts), Pass, Pass]));
        assert!(!matches(level_one, &[Suit(2, Hearts), Pass, Pass]));
        assert!(matches("^1[CD] P$", &[Suit(1, Diamonds), Pass]));
        assert!(!matches("^1[CD] P$", &[Suit(1, Hearts), Pass]));
        assert!(matches("^[1-3]S$", &[Suit(3, Spades)]));
        assert!(!matches("^[1-3]S$", &[Suit(4, Spades)]));
    }

    #[test]
    fn any_call() {
        let slam_ask = "^(P )*2C [PX] 2D ..? 4N$";
        assert!(matches(
            slam_ask,
            &[Suit(2, Clubs), Pass, Suit(2, Diamonds), Pass, NT(4)]
        ));
        assert!(matches(
            slam_ask,
            &[Suit(2, Clubs), Pass, Suit(2, Diamonds), NT(3), NT(4)]
        ));
        assert!(!matches(
            slam_ask,
            &[Suit(2, Clubs), Pass, Suit(2, Diamonds), NT(4)]
        ));
        assert!(matches(
            slam_ask,
            &[Suit(2, Clubs), Pass, Suit(2, Diamonds), Redouble, NT(4)]
        ));
    }

    #[test]
    fn the_end_of_any_auction() {
        assert!(matches("3N$", &[NT(3)]));
        assert!(matches("3N$", &[Suit(1, Clubs), Pass, NT(3)]));
        assert!(!matches("3N$", &[NT(3), Pass]));
        let any_contract = "[1-7][CDHSN]$";
        assert!(matches(any_contract, &[Pass, Pass, Suit(7, Spades)]));
        assert!(!matches(any_contract, &[Suit(7, Spades), Pass]));
    }

    #[test]
    fn a_gap_is_any_number_of_calls() {
        let raise = "[12]. [12]. .*2S$";
        assert!(matches(
            raise,
            &[Suit(1, Hearts), Suit(2, Clubs), Suit(2, Spades)]
        ));
        assert!(matches(
            raise,
            &[
                Pass,
                Suit(1, Hearts),
                Suit(2, Clubs),
                Pass,
                Pass,
                Suit(2, Spades)
            ]
        ));
        assert!(!matches(raise, &[Suit(1, Hearts), Pass, Suit(2, Spades)]));
        assert!(!matches(raise, &[Suit(2, Spades)]));
        assert!(!matches(
            raise,
            &[Suit(1, Hearts), Suit(3, Clubs), Suit(2, Spades)]
        ));
    }

    #[test]
    fn groups_of_alternatives_are_written_out() {
        let both = auctions("^(P )*(1N [PX] 2|2N [PX] 3)C$");
        let patterns = both.patterns();
        assert_eq!(2, patterns.len());
        assert_eq!(Some(Suit(2, Clubs)), patterns[0].last_bid());
        assert_eq!(Some(Suit(3, Clubs)), patterns[1].last_bid());
        assert!(both.matches(&[NT(1), Pass, Suit(2, Clubs)]));
        assert!(both.matches(&[NT(2), Double, Suit(3, Clubs)]));
        assert!(!both.matches(&[NT(2), Pass, Suit(2, Clubs)]));
        let middle = auctions("^2C (P 2D|X P) [PX] 2N$");
        assert!(middle.matches(&[Suit(2, Clubs), Pass, Suit(2, Diamonds), Pass, NT(2)]));
        assert!(middle.matches(&[Suit(2, Clubs), Double, Pass, Double, NT(2)]));
        assert!(!middle.matches(&[Suit(2, Clubs), Pass, Pass, Pass, NT(2)]));
    }

    #[test]
    fn the_last_bid_is_one_bid() {
        let last = |notation: &str| auctions(notation).patterns()[0].last_bid();
        assert_eq!(Some(Suit(2, Clubs)), last(r"^(P )*1N [PX] 2C$"));
        assert_eq!(Some(NT(4)), last(r"^(P )*2C [PX] 2D ..? 4N$"));
        assert_eq!(Some(Suit(3, Spades)), last(r"[123]. [123]. .*3S$"));
        assert_eq!(Some(Double), last(r"^(P )?[123]C X$"));
        assert_eq!(Some(Pass), last(r"^(P )*P$"));
        assert_eq!(None, last(r"^(P )*1N [PX] [23]C$"));
        assert_eq!(None, last("[1-7][CDHSN]$"));
    }

    #[test]
    fn alternatives_that_match_alike_are_one_pattern() {
        // Passes that may come first are the pattern's lead, not patterns.
        assert_eq!(1, auctions("^(P )?(P )?2H$").patterns().len());
        let optional = auctions("^1N (P )?2C$");
        assert_eq!(2, optional.patterns().len());
        assert!(optional.matches(&[NT(1), Suit(2, Clubs)]));
        assert!(optional.matches(&[NT(1), Pass, Suit(2, Clubs)]));
        assert!(!optional.matches(&[NT(1), Pass, Pass, Suit(2, Clubs)]));
        assert_eq!(1, Auctions::new(["^1N$", "^1N$"]).unwrap().patterns().len());
    }

    #[test]
    fn notation_that_is_not_understood_is_an_error() {
        for notation in [
            "^1N",
            "^1N|2N$",
            "^(1N$",
            "^1N)$",
            "^1N+$",
            "^(P )+1N$",
            "^1N (P )*2C$",
            "^1N [PX 2C$",
            "^1Z$",
            "^1N .*$",
            "^1N 2C $",
        ] {
            assert!(
                Auctions::new([notation]).is_err(),
                "{notation} was understood"
            );
        }
    }

    #[test]
    fn nothing_matches_no_auctions() {
        assert!(!Auctions::empty().matches(&[]));
        assert!(!Auctions::empty().matches(&[NT(1)]));
    }
}
