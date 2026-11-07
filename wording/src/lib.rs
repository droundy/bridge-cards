//! Build English sentences from small pieces that know their own grammar,
//! and display them as HTML, plain text or LaTeX.
//!
//! Gluing sentences together with `format!` goes wrong as soon as a count is
//! one ("1 spades"), a list has one item ("a, and b"), or a sentence starts
//! with a variable.  Here each piece decides its own grammar instead: a
//! [`Quantity`] picks the singular or plural of its [`Noun`], a [`Verb`] agrees
//! with a [`Number`], [`a`] picks "a" or "an", and a [`List`] joins its items.
//!
//! The pieces go into a [`Text`], which keeps words apart from markup until it
//! is displayed with [`display_as`].  Words are escaped for the format, and
//! markup, of a type `M` you choose, displays itself, so the same `Text` can be
//! HTML for a web page, UTF8 for a test and LaTeX for a printed sheet.
//!
//! ```
//! use display_as::{format_as, UTF8};
//! use wording::{count, text, List, Noun, Text, HAS};
//!
//! const POINT: Noun = Noun::new("point", "points");
//! let points = count(1u8, POINT);
//! let sentence: Text = text!(
//!     "partner ", HAS.agreeing(points.number()), " ", points, " and ",
//!     List::and(vec!["a singleton", "a void"]),
//! )
//! .sentence();
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
    a, at_least, at_most, between, count, Countable, Indefinite, Noun, Number, Quantity, Verb, HAS,
    IS,
};
pub use list::List;
pub use text::{AddTo, Markup, NoMarkup, Plain, Text};
