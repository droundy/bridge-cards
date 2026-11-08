//! A suit as `wording::Markup`: a noun that plain text spells out and HTML and
//! LaTeX show as its symbol.
use crate::Suit;
use display_as::{DisplayAs, HTML};
use std::fmt::{self, Formatter};
use wording::{Form, Markup, Number};

impl Markup for Suit {
    fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        <Suit as DisplayAs<HTML>>::fmt(self, f)
    }
    fn utf8(&self, form: Form, f: &mut Formatter) -> fmt::Result {
        let name = self.name();
        let name = match form.number {
            Number::Plural => name,
            Number::Singular => name.trim_end_matches('s'),
        };
        if form.capital {
            let mut chars = name.chars();
            let first = chars.next().into_iter().flat_map(char::to_uppercase);
            write!(f, "{}{}", first.collect::<String>(), chars.as_str())
        } else {
            f.write_str(name)
        }
    }
    fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Suit::Clubs => r"\clubsuit{}",
            Suit::Diamonds => r"\diamondsuit{}",
            Suit::Hearts => r"\heartsuit{}",
            Suit::Spades => r"\spadesuit{}",
        })
    }
    fn noun(&self) -> Option<Number> {
        Some(Number::Plural)
    }
}

wording::markup!(Suit);

#[test]
fn suits_agree_in_sentences() {
    let sentence = (5u8..) * Suit::Spades + wording::Word::IS + "trumps";
    assert_eq!(sentence.to_string(), "5+ spades are trumps");
    assert_eq!((1u8 * Suit::Hearts).to_string(), "1 heart");
}
