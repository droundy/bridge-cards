//! The same text displayed as plain text, HTML and LaTeX.
use display_as::{format_as, LaTeX, HTML, UTF8};
use expect_test::{expect, Expect};
use std::fmt::{self, Formatter};
use wording::{
    a, agree, and, at_least, at_most, between, count, numbered, or, text, then, Amount, Form,
    Markup, Number, Text, Word,
};

/// A playing card's suit as a noun, which plain text spells out and HTML and
/// LaTeX show as its symbol.
#[derive(Clone, Copy)]
enum Suit {
    Hearts,
    Spades,
}

const SPADES: Suit = Suit::Spades;
const HEARTS: Suit = Suit::Hearts;

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
}

impl Markup for Suit {
    fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(Suit::html(*self))
    }
    fn utf8(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        let name = match (self, form.number) {
            (Suit::Hearts, Number::Singular) => "heart",
            (Suit::Hearts, Number::Plural) => "hearts",
            (Suit::Spades, Number::Singular) => "spade",
            (Suit::Spades, Number::Plural) => "spades",
        };
        if form.capital {
            let mut chars = name.chars();
            let first = chars.next().unwrap().to_uppercase();
            write!(f, "{first}{}", chars.as_str())
        } else {
            f.write_str(name)
        }
    }
    fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(Suit::latex(*self))
    }
    fn noun(&self) -> Option<Number> {
        Some(Number::Plural)
    }
}

wording::countable!(Suit);

/// A bid such as "1♠", which is not a noun.
#[derive(Clone, Copy)]
struct Bid(u8, Suit);

impl Markup for Bid {
    fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1.html())
    }
    fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1.symbol())
    }
    fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1.latex())
    }
}

fn check(text: impl Into<Text>, expect: Expect) {
    let text: Text = text.into();
    let shown = format!(
        "{}\n{}\n{}",
        format_as!(UTF8, text).into_string(),
        format_as!(HTML, text).into_string(),
        format_as!(LaTeX, text).into_string(),
    );
    expect.assert_eq(&shown);
}

/// The phrases one after another, with `separator` between them.
fn joined(phrases: impl IntoIterator<Item = Text>, separator: &str) -> Text {
    let mut text = Text::new();
    for (i, phrase) in phrases.into_iter().enumerate() {
        if i > 0 {
            text += separator;
        }
        text += phrase;
    }
    text
}

const POINT: Word = Word::noun("point", "points");
const CARD: Word = Word::noun("card", "cards");
const LOSER: Word = Word::noun("loser", "losers");

#[test]
fn markup_and_escapes() {
    let hand = text!(Word::HAS, " ", 1 * text!("high card ", POINT), " & ")
        + (5..) * SPADES
        + " in 100% of "
        + Bid(1, Suit::Spades)
        + " openings";
    check(
        agree(numbered("partner", Number::Singular), hand).sentence(),
        expect![[r#"
            Partner has 1 high card point & 5+ spades in 100% of 1♠ openings.
            Partner has 1 high card point &amp; 5+ <span class="spades">♠</span> in 100% of 1<span class="spades">♠</span> openings.
            Partner has 1 high card point \& 5+ \spadesuit{} in 100\% of 1\spadesuit{} openings."#]],
    );
}

#[test]
fn quantities_agree() {
    fn enough(quantity: Text) -> Text {
        agree(quantity, text!(Word::IS, " enough"))
    }
    check(
        joined(
            vec![
                enough(count(7, CARD)),
                enough(count(0, CARD)),
                enough(at_least(1, CARD)),
                enough(at_most(1, CARD)),
                enough(at_most(0, CARD)),
                enough(between(6, 9, text!("high card ", POINT))),
                enough(between(4, 4, CARD)),
                enough(between(1, 1, CARD)),
                enough(between(0, 3, CARD)),
                enough(count(Amount::halves(13), LOSER)),
                enough(count(Amount::halves(2), LOSER)),
                enough(count(-1, CARD)),
            ],
            "; ",
        ),
        expect![[r#"
            7 cards are enough; no cards are enough; 1+ cards are enough; at most 1 card is enough; no cards are enough; 6–9 high card points are enough; exactly 4 cards are enough; exactly 1 card is enough; at most 3 cards are enough; 6½ losers are enough; 1 loser is enough; -1 cards are enough
            7 cards are enough; no cards are enough; 1+ cards are enough; at most 1 card is enough; no cards are enough; 6–9 high card points are enough; exactly 4 cards are enough; exactly 1 card is enough; at most 3 cards are enough; 6½ losers are enough; 1 loser is enough; -1 cards are enough
            7 cards are enough; no cards are enough; 1+ cards are enough; at most 1 card is enough; no cards are enough; 6–9 high card points are enough; exactly 4 cards are enough; exactly 1 card is enough; at most 3 cards are enough; 6½ losers are enough; 1 loser is enough; -1 cards are enough"#]],
    );
}

#[test]
fn only_the_head_noun_agrees() {
    let n: u8 = 2;
    check(
        joined(
            vec![
                1 * SPADES,
                (5..) * SPADES,
                (..=1) * HEARTS,
                (n..=n + 1) * HEARTS,
                3 * text!("long ", SPADES),
                1 * text!(CARD, " in ", HEARTS),
                count(7, text!("high card ", POINT, " in ", SPADES)),
            ],
            "; ",
        ),
        expect![[r#"
            1 spade; 5+ spades; at most 1 heart; 2–3 hearts; 3 long spades; 1 card in hearts; 7 high card points in spades
            1 <span class="spades">♠</span>; 5+ <span class="spades">♠</span>; at most 1 <span class="hearts">♥</span>; 2–3 <span class="hearts">♥</span>; 3 long <span class="spades">♠</span>; 1 card in <span class="hearts">♥</span>; 7 high card points in <span class="spades">♠</span>
            1 \spadesuit{}; 5+ \spadesuit{}; at most 1 \heartsuit{}; 2–3 \heartsuit{}; 3 long \spadesuit{}; 1 card in \heartsuit{}; 7 high card points in \spadesuit{}"#]],
    );
}

#[test]
fn subjects_agree() {
    let partner = || numbered("partner", Number::Singular);
    let you = || numbered("you", Number::Plural);
    let five_spades = (5..) * SPADES;
    check(
        joined(
            vec![
                agree(
                    and([
                        numbered("North", Number::Singular),
                        numbered("South", Number::Singular),
                    ]),
                    text!(Word::HAS, " bid"),
                ),
                agree(or([partner(), you()]), text!(Word::IS, " declarer")),
                agree(SPADES, text!(Word::IS, " trumps")),
                agree(partner(), text!(Word::IS, " short in ", HEARTS)),
                agree(
                    you(),
                    text!(Word::HAS, " opened and ", Word::IS, " forcing"),
                ),
                agree(partner(), text!(Word::HAS, " ", five_spades.clone())),
                agree(five_spades, text!(Word::IS, " enough")),
                agree(
                    you(),
                    text!("know that ", agree(partner(), text!(Word::HAS, " bid"))),
                ),
            ],
            "; ",
        ),
        expect![[r#"
            North and South have bid; partner or you are declarer; spades are trumps; partner is short in hearts; you have opened and are forcing; partner has 5+ spades; 5+ spades are enough; you know that partner has bid
            North and South have bid; partner or you are declarer; <span class="spades">♠</span> are trumps; partner is short in <span class="hearts">♥</span>; you have opened and are forcing; partner has 5+ <span class="spades">♠</span>; 5+ <span class="spades">♠</span> are enough; you know that partner has bid
            North and South have bid; partner or you are declarer; \spadesuit{} are trumps; partner is short in \heartsuit{}; you have opened and are forcing; partner has 5+ \spadesuit{}; 5+ \spadesuit{} are enough; you know that partner has bid"#]],
    );
}

#[test]
fn noun_phrases_keep_their_number() {
    const HAND: Word = Word::noun("hand", "hands");
    const SUIT: Word = Word::noun("suit", "suits");
    let partner = || numbered("partner", Number::Singular);
    let you = || numbered("you", Number::Plural);
    // Each phrase reads the same whoever holds it.
    let balanced = || a(text!(HAND, " that ", Word::IS, " balanced"));
    let high = || count(5, text!(CARD, " that ", Word::IS, " high"));
    check(
        joined(
            vec![
                agree(you(), text!(Word::HAS, " ", balanced())),
                agree(partner(), text!(Word::HAS, " ", balanced())),
                agree(you(), text!(Word::HAS, " ", high())),
                agree(partner(), text!(Word::HAS, " ", high())),
                agree(high(), text!(Word::IS, " enough")),
                // A clause with a subject of its own keeps its own verbs.
                agree(
                    you(),
                    text!(
                        Word::HAS,
                        " ",
                        a(text!(
                            SUIT,
                            " that ",
                            agree(partner(), text!(Word::HAS, " bid"))
                        ))
                    ),
                ),
                // The first noun or noun phrase decides a subject's number.
                agree(
                    a(text!(HAND, " with ", (5..) * SPADES)),
                    text!(Word::IS, " strong"),
                ),
                agree(
                    text!("the ", CARD, " with ", count(5, POINT)),
                    text!(Word::IS, " led"),
                ),
                agree(text!("your ", count(7, POINT)), text!(Word::IS, " enough")),
            ],
            "; ",
        ),
        expect![[r#"
            you have a hand that is balanced; partner has a hand that is balanced; you have 5 cards that are high; partner has 5 cards that are high; 5 cards that are high are enough; you have a suit that partner has bid; a hand with 5+ spades is strong; the card with 5 points is led; your 7 points are enough
            you have a hand that is balanced; partner has a hand that is balanced; you have 5 cards that are high; partner has 5 cards that are high; 5 cards that are high are enough; you have a suit that partner has bid; a hand with 5+ <span class="spades">♠</span> is strong; the card with 5 points is led; your 7 points are enough
            you have a hand that is balanced; partner has a hand that is balanced; you have 5 cards that are high; partner has 5 cards that are high; 5 cards that are high are enough; you have a suit that partner has bid; a hand with 5+ \spadesuit{} is strong; the card with 5 points is led; your 7 points are enough"#]],
    );
}

#[test]
fn lists() {
    assert!(and(Vec::<&str>::new()).is_empty());
    check(
        joined(
            [
                and(["a"]),
                and(["a", "b"]),
                and(["a", "b", "c"]),
                or(["a", "b", "c"]),
                then([
                    text!("opened ", Bid(1, Suit::Spades)),
                    text!("bid ", Bid(2, Suit::Hearts)),
                ]),
            ],
            " | ",
        ),
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
        "honor",
        "one-level bid",
        "onerous contract",
        "unit",
        "unique bid",
        "unimportant card",
        "useful bid",
        "umbrella",
        "European",
        "8-card fit",
        "11-count",
        "18-point hand",
        "1NT opening",
        "NT contract",
        "HCP count",
        "SAYC bid",
    ];
    let mut phrases: Vec<Text> = words.iter().map(|&w| a(w)).collect();
    phrases.push(a(Bid(1, Suit::Spades)));
    phrases.push(a(text!(SPADES, " bid")));
    phrases.push(a(text!(CARD, " in ", HEARTS)));
    check(
        and(phrases),
        expect![[r#"
            an ace, a king, an eight, an hour, an honest player, an honor, a one-level bid, an onerous contract, a unit, a unique bid, an unimportant card, a useful bid, an umbrella, a European, an 8-card fit, an 11-count, an 18-point hand, a 1NT opening, an NT contract, an HCP count, a SAYC bid, a 1♠, a spade bid and a card in hearts
            an ace, a king, an eight, an hour, an honest player, an honor, a one-level bid, an onerous contract, a unit, a unique bid, an unimportant card, a useful bid, an umbrella, a European, an 8-card fit, an 11-count, an 18-point hand, a 1NT opening, an NT contract, an HCP count, a SAYC bid, a 1<span class="spades">♠</span>, a <span class="spades">♠</span> bid and a card in <span class="hearts">♥</span>
            an ace, a king, an eight, an hour, an honest player, an honor, a one-level bid, an onerous contract, a unit, a unique bid, an unimportant card, a useful bid, an umbrella, a European, an 8-card fit, an 11-count, an 18-point hand, a 1NT opening, an NT contract, an HCP count, a SAYC bid, a 1\spadesuit{}, a \spadesuit{} bid and a card in \heartsuit{}"#]],
    );
}

#[test]
fn sentences() {
    let mut forcing = text!("is that ");
    forcing += "forcing?";
    check(
        joined(
            [
                text!("bid it").sentence(),
                forcing.sentence(),
                text!(Bid(1, Suit::Spades), " is natural").sentence(),
                text!(None::<&str>, "nothing optional").sentence(),
                text!(Word::HAS, " it").capitalized(),
                text!(String::new(), Word::HAS, " it").sentence(),
                text!("bid it. ").sentence(),
                text!("say \"pass.\"").sentence(),
                text!("(", "maybe)").sentence(),
                agree(SPADES, text!(Word::IS, " trumps")).sentence(),
                text!(a("ace"), " is high").sentence(),
            ],
            " ",
        ),
        expect![[r#"
            Bid it. Is that forcing? 1♠ is natural. Nothing optional. Has it Has it. Bid it. Say "pass." (Maybe). Spades are trumps. An ace is high.
            Bid it. Is that forcing? 1<span class="spades">♠</span> is natural. Nothing optional. Has it Has it. Bid it. Say &quot;pass.&quot; (Maybe). <span class="spades">♠</span> are trumps. An ace is high.
            Bid it. Is that forcing? 1\spadesuit{} is natural. Nothing optional. Has it Has it. Bid it. Say "pass." (Maybe). \spadesuit{} are trumps. An ace is high."#]],
    );
}

#[test]
fn text_is_send_and_sync() {
    // So that a server can keep text in shared state or across an `.await`.
    fn send_and_sync<T: Send + Sync>() {}
    send_and_sync::<Text>();
}
