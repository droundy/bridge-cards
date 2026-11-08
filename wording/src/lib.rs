//! Build English sentences from small pieces that know their own grammar,
//! and display them as HTML, plain text or LaTeX.
//!
//! Gluing sentences together with `format!` goes wrong as soon as a count is
//! one ("1 spades"), a list has one item ("a, and b"), or a sentence starts
//! with a variable.  Here each piece decides its own grammar instead: a
//! quantity such as [`count`] makes its noun agree with its amount, [`agree`]
//! makes the verbs of a predicate agree with their subject, [`a`] picks "a"
//! or "an", and [`and`], [`or`] and [`then`] join items into a list.  A
//! [`Word`] is a noun such as "point" or a verb such as [`Word::HAS`], with a
//! singular and a plural form.
//!
//! Every phrase is a [`Text`], so any phrase can be used inside any other,
//! and a phrase reads the same wherever it goes.  Each function takes its
//! pieces as anything that converts `Into<Text>`, and returns a `Text`.  A `Text` keeps words apart
//! from [`Markup`] until it is displayed with [`display_as`] (or as plain
//! text with `Display`).  Words are escaped for the format, and markup, such
//! as a playing card that HTML shows in colour, displays itself.
//!
//! ```
//! use wording::{a, agree, and, numbered, text, Number, Word};
//!
//! const POINT: Word = Word::noun("point", "points");
//! let partner = numbered("partner", Number::Singular);
//! let hand = and([1 * POINT, a("singleton"), a("void")]);
//! let sentence = agree(partner, text!(Word::HAS, " ", hand)).sentence();
//! assert_eq!(sentence.to_string(), "Partner has 1 point, a singleton and a void.");
//! ```
#![deny(missing_docs)]

mod grammar;
mod list;
mod text;

pub use grammar::{a, agree, at_least, at_most, between, count, numbered, Amount, Number, Word};
pub use list::{and, or, then};
pub use text::{Form, Markup, Text};
