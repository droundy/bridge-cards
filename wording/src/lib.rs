//! Build English sentences from small pieces that know their own grammar,
//! and display them as HTML, plain text or LaTeX.
//!
//! Gluing sentences together with `format!` goes wrong as soon as a count is
//! one ("1 spades"), a list has one item ("a, and b"), or a sentence starts
//! with a variable.  Here each piece decides its own grammar instead: a
//! [`Quantity`] makes its noun agree with its amount, [`agree`] makes a verb
//! agree with its [`Subject`], [`a`] picks "a" or "an", and a [`List`] joins
//! its items.  A [`Word`] is the plain-words piece that agrees, such as
//! "point" or [`HAS`].
//!
//! The pieces go into a [`Text`], which keeps words apart from markup until it
//! is displayed with [`display_as`].  Words are escaped for the format, and
//! markup, of a type `M` you choose, displays itself, so the same `Text` can be
//! HTML for a web page, UTF8 for a test and LaTeX for a printed sheet.  Markup
//! that agrees with a plural displays in the format [`Plural<F>`](Plural).
//!
//! ```
//! use display_as::{format_as, UTF8};
//! use wording::{agree, numbered, text, List, Number, Text, Word, HAS};
//!
//! const POINT: Word = Word::new("point", "points");
//! let partner = numbered("partner", Number::Singular);
//! let hand = text!(HAS, " ", 1 * POINT, " and ")
//!     + List::and(vec!["a singleton", "a void"]);
//! let sentence: Text = agree(partner, hand).sentence();
//! assert_eq!(
//!     format_as!(UTF8, sentence).into_string(),
//!     "Partner has 1 point and a singleton and a void."
//! );
//! ```
#![deny(missing_docs)]

mod grammar;
mod list;
mod text;

pub use grammar::{
    a, agree, at_least, at_most, between, count, numbered, Indefinite, Number, Numbered, Quantity,
    Subject, Word, HAS, IS,
};
pub use list::List;
pub use text::{AddTo, Markup, NoMarkup, Plain, Plural, Text};
