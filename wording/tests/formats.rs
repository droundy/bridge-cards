//! The same text displayed as plain text, HTML and LaTeX.
use display_as::{format_as, DisplayAs, LaTeX, HTML, UTF8};
use expect_test::{expect, Expect};
use std::fmt::{Error, Formatter};
use wording::{
    a, agree, at_least, at_most, between, count, numbered, text, AddTo, List, Number, Plural,
    Quantity, Text, Word, HAS, IS,
};

/// A playing card's suit.
#[derive(Clone, Copy)]
enum Suit {
    Hearts,
    Spades,
}

impl Suit {
    fn symbol(self) -> &'static str {
        match self {
            Suit::Hearts => "♥",
            Suit::Spades => "♠",
        }
    }
    fn html(self) -> &'static str {
        match self {
            Suit::Hearts => r#"<span class="hearts">♥</span>"#,
            Suit::Spades => r#"<span class="spades">♠</span>"#,
        }
    }
    fn latex(self) -> &'static str {
        match self {
            Suit::Hearts => r"\heartsuit{}",
            Suit::Spades => r"\spadesuit{}",
        }
    }
    fn name(self, number: Number) -> &'static str {
        let word = match self {
            Suit::Hearts => ("heart", "hearts"),
            Suit::Spades => ("spade", "spades"),
        };
        match number {
            Number::Singular => word.0,
            Number::Plural => word.1,
        }
    }
}

/// The markup: a bid such as "1♠", or a suit as a noun, which plain text
/// spells out and HTML and LaTeX show as its symbol.
#[derive(Clone, Copy)]
enum Mark {
    Bid(u8, Suit),
    Suit(Suit),
}

const SPADES: Mark = Mark::Suit(Suit::Spades);
const HEARTS: Mark = Mark::Suit(Suit::Hearts);

impl DisplayAs<UTF8> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Mark::Bid(level, suit) => write!(f, "{level}{}", suit.symbol()),
            Mark::Suit(suit) => f.write_str(suit.name(Number::Singular)),
        }
    }
}

impl DisplayAs<Plural<UTF8>> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Mark::Suit(suit) => f.write_str(suit.name(Number::Plural)),
            bid => <Mark as DisplayAs<UTF8>>::fmt(&bid, f),
        }
    }
}

impl DisplayAs<HTML> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Mark::Bid(level, suit) => write!(f, "{level}{}", suit.html()),
            Mark::Suit(suit) => f.write_str(suit.html()),
        }
    }
}

impl DisplayAs<Plural<HTML>> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        <Mark as DisplayAs<HTML>>::fmt(self, f)
    }
}

impl DisplayAs<LaTeX> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Mark::Bid(level, suit) => write!(f, "{level}{}", suit.latex()),
            Mark::Suit(suit) => f.write_str(suit.latex()),
        }
    }
}

impl DisplayAs<Plural<LaTeX>> for Mark {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        <Mark as DisplayAs<LaTeX>>::fmt(self, f)
    }
}

impl AddTo<Mark> for Mark {
    fn add_to(self, text: &mut Text<Mark>) {
        text.push_markup(self);
    }
}

fn check(text: &Text<Mark>, expect: Expect) {
    let shown = format!(
        "{}\n{}\n{}",
        format_as!(UTF8, text).into_string(),
        format_as!(HTML, text).into_string(),
        format_as!(LaTeX, text).into_string(),
    );
    expect.assert_eq(&shown);
}

const POINT: Word = Word::new("point", "points");
const CARD: Word = Word::new("card", "cards");
const LOSER: Word = Word::new("loser", "losers");

#[test]
fn markup_and_escapes() {
    let hand = text!(HAS, " ", count(1u8, text!("high card ", POINT)), " & ")
        + at_least(5u8, SPADES)
        + " in 100% of "
        + Mark::Bid(1, Suit::Spades)
        + " openings";
    check(
        &agree(numbered("partner", Number::Singular), hand).sentence(),
        expect![[r#"
            Partner has 1 high card point & 5+ spades in 100% of 1♠ openings.
            Partner has 1 high card point &amp; 5+ <span class="spades">♠</span> in 100% of 1<span class="spades">♠</span> openings.
            Partner has 1 high card point \& 5+ \spadesuit{} in 100\% of 1\spadesuit{} openings."#]],
    );
}

#[test]
fn quantities_agree() {
    let quantities = vec![
        count(7u8, CARD),
        count(0u8, CARD),
        at_least(1u8, CARD),
        at_most(1u8, CARD),
        between(6u8, 9u8, text!("high card ", POINT)),
        Quantity::other("6½", Number::Plural, LOSER),
    ];
    let phrases: Vec<Text<Mark>> = quantities
        .into_iter()
        .map(|q| agree(q, text!(IS, " enough")))
        .collect();
    check(
        &text!(List::with(phrases, "; ", "; ")),
        expect![[r#"
        7 cards are enough; 0 cards are enough; 1+ cards are enough; at most 1 card is enough; 6–9 high card points are enough; 6½ losers are enough
        7 cards are enough; 0 cards are enough; 1+ cards are enough; at most 1 card is enough; 6–9 high card points are enough; 6½ losers are enough
        7 cards are enough; 0 cards are enough; 1+ cards are enough; at most 1 card is enough; 6–9 high card points are enough; 6½ losers are enough"#]],
    );
}

#[test]
fn quantities_of_markup() {
    let phrases: Vec<Text<Mark>> = vec![
        text!(count(1u8, SPADES)),
        text!(at_least(5u8, SPADES)),
        text!(at_most(1u8, HEARTS)),
        text!(between(2u8, 3u8, HEARTS)),
    ];
    check(
        &text!(List::with(phrases, "; ", "; ")),
        expect![[r#"
        1 spade; 5+ spades; at most 1 heart; 2–3 hearts
        1 <span class="spades">♠</span>; 5+ <span class="spades">♠</span>; at most 1 <span class="hearts">♥</span>; 2–3 <span class="hearts">♥</span>
        1 \spadesuit{}; 5+ \spadesuit{}; at most 1 \heartsuit{}; 2–3 \heartsuit{}"#]],
    );
}

#[test]
fn subjects_agree() {
    let north_south = List::and(vec![
        numbered("North", Number::Singular),
        numbered("South", Number::Singular),
    ]);
    let partner_or_you = List::or(vec![
        numbered("partner", Number::Singular),
        numbered("you", Number::Plural),
    ]);
    let clauses: Vec<Text<Mark>> = vec![
        agree(north_south, text!(HAS, " bid")),
        agree(partner_or_you, text!(IS, " declarer")),
        agree(numbered(SPADES, Number::Plural), text!(IS, " trumps")),
        agree(
            numbered("the opening", Number::Singular),
            text!(IS, " ", at_least(5u8, SPADES)),
        ),
    ];
    check(
        &text!(List::with(clauses, "; ", "; ")),
        expect![[r#"
        North and South have bid; partner or you are declarer; spades are trumps; the opening is 5+ spades
        North and South have bid; partner or you are declarer; <span class="spades">♠</span> are trumps; the opening is 5+ <span class="spades">♠</span>
        North and South have bid; partner or you are declarer; \spadesuit{} are trumps; the opening is 5+ \spadesuit{}"#]],
    );
}

#[test]
fn lists() {
    let empty: Text<Mark> = text!(List::and(Vec::<&str>::new()));
    assert!(empty.is_empty());
    let lists: Vec<Text<Mark>> = vec![
        text!(List::and(vec!["a"])),
        text!(List::and(vec!["a", "b"])),
        text!(List::and(vec!["a", "b", "c"])),
        text!(List::or(vec!["a", "b", "c"])),
        text!(List::then(vec![
            text!("opened ", Mark::Bid(1, Suit::Spades)),
            text!("bid ", Mark::Bid(2, Suit::Hearts))
        ])),
    ];
    check(
        &text!(List::with(lists, " | ", " | ")),
        expect![[r#"
            a | a and b | a, b and c | a, b or c | opened 1♠, then bid 2♥
            a | a and b | a, b and c | a, b or c | opened 1<span class="spades">♠</span>, then bid 2<span class="hearts">♥</span>
            a | a and b | a, b and c | a, b or c | opened 1\spadesuit{}, then bid 2\heartsuit{}"#]],
    );
}

#[test]
fn articles() {
    let words = [
        "ace",
        "king",
        "eight",
        "hour",
        "honest player",
        "one-level bid",
        "unit",
        "useful bid",
        "umbrella",
        "European",
    ];
    let phrases: Vec<_> = words.iter().map(|&w| a(w)).collect();
    check(
        &text!(List::and(phrases)),
        expect![[r#"
            an ace, a king, an eight, an hour, an honest player, a one-level bid, a unit, a useful bid, an umbrella and a European
            an ace, a king, an eight, an hour, an honest player, a one-level bid, a unit, a useful bid, an umbrella and a European
            an ace, a king, an eight, an hour, an honest player, a one-level bid, a unit, a useful bid, an umbrella and a European"#]],
    );
}

#[test]
fn sentences() {
    let mut forcing: Text<Mark> = text!("is that ");
    forcing += "forcing?";
    let sentences: Vec<Text<Mark>> = vec![
        text!("bid it").sentence(),
        forcing.sentence(),
        text!(Mark::Bid(1, Suit::Spades), " is natural").sentence(),
        text!(None::<&str>, "nothing optional").sentence(),
        text!(HAS, " it").capitalized(),
    ];
    check(
        &text!(List::with(sentences, " ", " ")),
        expect![[r#"
        Bid it. Is that forcing? 1♠ is natural. Nothing optional. Has it
        Bid it. Is that forcing? 1<span class="spades">♠</span> is natural. Nothing optional. Has it
        Bid it. Is that forcing? 1\spadesuit{} is natural. Nothing optional. Has it"#]],
    );
}
