//! The same text displayed as plain text, HTML and LaTeX.
use display_as::{format_as, DisplayAs, LaTeX, HTML, UTF8};
use expect_test::{expect, Expect};
use std::fmt::{Error, Formatter};
use wording::{
    a, at_least, at_most, between, count, text, AddTo, List, Noun, Number, Quantity, Text, HAS, IS,
};

/// A playing card's suit, which each format shows its own way.
#[derive(Clone, Copy)]
enum Suit {
    Hearts,
    Spades,
}

impl DisplayAs<UTF8> for Suit {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(match self {
            Suit::Hearts => "♥",
            Suit::Spades => "♠",
        })
    }
}

impl DisplayAs<HTML> for Suit {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(match self {
            Suit::Hearts => r#"<span class="hearts">♥</span>"#,
            Suit::Spades => r#"<span class="spades">♠</span>"#,
        })
    }
}

impl DisplayAs<LaTeX> for Suit {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(match self {
            Suit::Hearts => r"\heartsuit{}",
            Suit::Spades => r"\spadesuit{}",
        })
    }
}

impl AddTo<Suit> for Suit {
    fn add_to(self, text: &mut Text<Suit>) {
        text.push_markup(self);
    }
}

fn check(text: &Text<Suit>, expect: Expect) {
    let shown = format!(
        "{}\n{}\n{}",
        format_as!(UTF8, text).into_string(),
        format_as!(HTML, text).into_string(),
        format_as!(LaTeX, text).into_string(),
    );
    expect.assert_eq(&shown);
}

const POINT: Noun = Noun::new("high card point", "high card points");
const CARD: Noun = Noun::new("card", "cards");
const LOSER: Noun = Noun::new("loser", "losers");

#[test]
fn markup_and_escapes() {
    let points = count(1u8, POINT);
    check(
        &text!(
            "partner ",
            HAS.agreeing(points.number()),
            " ",
            points,
            " & 5+ ",
            Suit::Spades,
            ", not 100% ",
            Suit::Hearts,
        )
        .sentence(),
        expect![[r#"
            Partner has 1 high card point & 5+ ♠, not 100% ♥.
            Partner has 1 high card point &amp; 5+ <span class="spades">♠</span>, not 100% <span class="hearts">♥</span>.
            Partner has 1 high card point \& 5+ \spadesuit{}, not 100\% \heartsuit{}."#]],
    );
}

#[test]
fn quantities_agree() {
    let quantities = vec![
        count(7u8, CARD),
        count(0u8, CARD),
        at_least(1u8, CARD),
        at_most(1u8, CARD),
        between(6u8, 9u8, POINT),
        Quantity::other("6½", Number::Plural, LOSER),
    ];
    let phrases: Vec<Text<Suit>> = quantities
        .into_iter()
        .map(|q| text!(IS.agreeing(q.number()), " ", q))
        .collect();
    check(
        &text!(List::with(phrases, "; ", "; ")),
        expect![[r#"
            are 7 cards; are 0 cards; are 1+ cards; is at most 1 card; are 6–9 high card points; are 6½ losers
            are 7 cards; are 0 cards; are 1+ cards; is at most 1 card; are 6–9 high card points; are 6½ losers
            are 7 cards; are 0 cards; are 1+ cards; is at most 1 card; are 6–9 high card points; are 6½ losers"#]],
    );
}

#[test]
fn lists() {
    let empty: Text<Suit> = text!(List::and(Vec::<&str>::new()));
    assert!(empty.is_empty());
    let lists: Vec<Text<Suit>> = vec![
        text!(List::and(vec!["a"])),
        text!(List::and(vec!["a", "b"])),
        text!(List::and(vec!["a", "b", "c"])),
        text!(List::or(vec!["a", "b", "c"])),
        text!(List::then(vec![
            text!("opened 1", Suit::Spades),
            text!("bid 2", Suit::Hearts)
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
    let sentences: Vec<Text<Suit>> = vec![
        text!("bid it").sentence(),
        text!("is that forcing?").sentence(),
        text!(Suit::Spades, " are trumps").sentence(),
        text!(None::<&str>, "nothing optional").sentence(),
    ];
    check(
        &text!(List::with(sentences, " ", " ")),
        expect![[r#"
            Bid it. Is that forcing? ♠ are trumps. Nothing optional.
            Bid it. Is that forcing? <span class="spades">♠</span> are trumps. Nothing optional.
            Bid it. Is that forcing? \spadesuit{} are trumps. Nothing optional."#]],
    );
}
