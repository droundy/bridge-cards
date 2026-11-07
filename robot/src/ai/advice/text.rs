//! Text that reads the same as HTML or as plain text.
//!
//! The `format_as!` and `with_template` macros cannot be generic over the
//! format, so the advice builds its sentences as `Text`: words, bids and suits
//! kept apart until they are displayed, so that HTML can colour the suits.
//! Phrases such as "5+ spades" are plain `Display` types that choose their
//! own grammar, and `List` joins them.
use crate::Bid;
use bridge_deck::Suit;
use display_as::{DisplayAs, Format, FormattedString, HTML, UTF8};
use std::fmt::{Error, Formatter};

/// Words, bids and suits, displayed as HTML or plain text.
#[derive(Clone, Default)]
pub(super) struct Text(Vec<Piece>);

#[derive(Clone)]
enum Piece {
    Words(String),
    Bid(Bid),
    Suit(Suit),
    /// Text from a convention table, which is already HTML.
    Html(FormattedString<HTML>),
}

/// Text made of words, bids and anything else that can be added to text, as
/// in `text!("Partner opened ", bid, ".")`.
macro_rules! text {
    ($($piece:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut text = Text::default();
        $( AddTo::add_to($piece, &mut text); )*
        text
    }};
}

/// Something that can be added to `Text`.
pub(super) trait AddTo {
    fn add_to(self, text: &mut Text);
}

impl Text {
    pub(super) fn push_words(&mut self, words: &str) {
        match self.0.last_mut() {
            Some(Piece::Words(w)) => w.push_str(words),
            _ => self.0.push(Piece::Words(words.to_string())),
        }
    }
    /// HTML from a convention table.
    pub(super) fn html(html: &FormattedString<HTML>) -> Self {
        Text(vec![Piece::Html(html.clone())])
    }
}

/// Plain values are added as their words.
macro_rules! add_as_words {
    ($($t:ty),*) => {
        $(impl AddTo for $t {
            fn add_to(self, text: &mut Text) {
                text.push_words(&self.to_string());
            }
        })*
    };
}
add_as_words!(&str, String, u8, usize);

impl AddTo for Bid {
    fn add_to(self, text: &mut Text) {
        text.0.push(Piece::Bid(self));
    }
}
impl AddTo for Suit {
    fn add_to(self, text: &mut Text) {
        text.0.push(Piece::Suit(self));
    }
}
impl AddTo for Text {
    fn add_to(self, text: &mut Text) {
        for piece in self.0 {
            match piece {
                Piece::Words(w) => text.push_words(&w),
                p => text.0.push(p),
            }
        }
    }
}

/// Items joined in a list, as in "a, b and c".
pub(super) struct List<T> {
    items: Vec<T>,
    separator: &'static str,
    last: &'static str,
}

impl<T> List<T> {
    /// "a", "a and b", "a, b and c".
    pub(super) fn and(items: Vec<T>) -> Self {
        List {
            items,
            separator: ", ",
            last: " and ",
        }
    }
    /// "a, then b, then c".
    pub(super) fn then(items: Vec<T>) -> Self {
        List {
            items,
            separator: ", then ",
            last: ", then ",
        }
    }
}

impl<T: AddTo> AddTo for List<T> {
    fn add_to(self, text: &mut Text) {
        let n = self.items.len();
        for (i, item) in self.items.into_iter().enumerate() {
            if i + 1 == n && i > 0 {
                text.push_words(self.last);
            } else if i > 0 {
                text.push_words(self.separator);
            }
            item.add_to(text);
        }
    }
}

impl DisplayAs<HTML> for Text {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for piece in self.0.iter() {
            match piece {
                Piece::Words(w) => HTML::escape(f, w)?,
                Piece::Bid(b) => DisplayAs::<HTML>::fmt(b, f)?,
                Piece::Suit(s) => DisplayAs::<HTML>::fmt(s, f)?,
                Piece::Html(h) => f.write_str(h.as_str())?,
            }
        }
        Ok(())
    }
}

impl DisplayAs<UTF8> for Text {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for piece in self.0.iter() {
            match piece {
                Piece::Words(w) => f.write_str(w)?,
                Piece::Bid(b) => DisplayAs::<UTF8>::fmt(b, f)?,
                Piece::Suit(s) => DisplayAs::<UTF8>::fmt(s, f)?,
                Piece::Html(h) => f.write_str(&plain(h.as_str()))?,
            }
        }
        Ok(())
    }
}

/// HTML as plain text: without its tags, and with its escapes undone.
fn plain(html: &str) -> String {
    let mut text = String::new();
    let mut in_tag = false;
    for c in html.chars() {
        match c {
            '<' => in_tag = true,
            '>' => in_tag = false,
            c if !in_tag => text.push(c),
            _ => {}
        }
    }
    [
        ("&lt;", "<"),
        ("&gt;", ">"),
        ("&quot;", "\""),
        ("&#x27;", "'"),
        ("&#x2f;", "/"),
        ("&amp;", "&"),
    ]
    .iter()
    .fold(text, |text, (escaped, c)| text.replace(escaped, c))
}
