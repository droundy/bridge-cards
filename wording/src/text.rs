use display_as::{DisplayAs, Format};
use std::fmt::{Display, Error, Formatter};

/// Words and markup, displayed in any format the markup supports.
///
/// Words are escaped for the format they are displayed in.  Markup is a
/// value of type `M`, such as a playing card that HTML shows in colour, and
/// displays itself through [`DisplayAs`].  Build a `Text` with [`text!`](crate::text!).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Text<M = NoMarkup>(Vec<Piece<M>>);

#[derive(Clone, Debug, PartialEq, Eq)]
enum Piece<M> {
    Words(String),
    Markup(M),
}

/// Markup for text that has none.  It has no values, so it displays in
/// every format.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NoMarkup {}

impl<F: Format> DisplayAs<F> for NoMarkup {
    fn fmt(&self, _: &mut Formatter) -> Result<(), Error> {
        match *self {}
    }
}

/// Text made of words, quantities, lists and markup: anything that
/// implements [`AddTo`].
///
/// ```
/// use display_as::{format_as, HTML};
/// use wording::{text, Text};
///
/// let name = "Smith & Wesson";
/// let t: Text = text!("Made by ", name, ".");
/// assert_eq!(format_as!(HTML, t).into_string(), "Made by Smith &amp; Wesson.");
/// ```
#[macro_export]
macro_rules! text {
    ($($piece:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut text = $crate::Text::new();
        $( $crate::AddTo::add_to($piece, &mut text); )*
        text
    }};
}

impl<M> Default for Text<M> {
    fn default() -> Self {
        Text(Vec::new())
    }
}

impl<M> Text<M> {
    /// Empty text.
    pub fn new() -> Self {
        Text::default()
    }
    /// Adds words to the end.
    pub fn push_words(&mut self, words: &str) {
        match self.0.last_mut() {
            Some(Piece::Words(w)) => w.push_str(words),
            _ => self.0.push(Piece::Words(words.to_string())),
        }
    }
    /// Adds markup to the end.
    pub fn push_markup(&mut self, markup: M) {
        self.0.push(Piece::Markup(markup));
    }
    /// Is there nothing to display?
    pub fn is_empty(&self) -> bool {
        self.0
            .iter()
            .all(|p| matches!(p, Piece::Words(w) if w.is_empty()))
    }
    /// The text with a capital first letter, if it starts with words.
    pub fn capitalized(mut self) -> Self {
        if let Some(Piece::Words(w)) = self.0.first_mut() {
            let mut chars = w.chars();
            if let Some(c) = chars.next() {
                *w = c.to_uppercase().chain(chars).collect();
            }
        }
        self
    }
    /// The text as a sentence: capitalized, and ending in a full stop unless
    /// it already ends in one, a question mark or an exclamation mark.
    pub fn sentence(mut self) -> Self {
        let ended = match self.0.last() {
            Some(Piece::Words(w)) => w.ends_with(['.', '?', '!']),
            _ => false,
        };
        if !ended {
            self.push_words(".");
        }
        self.capitalized()
    }
}

impl<F: Format, M: DisplayAs<F>> DisplayAs<F> for Text<M> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for piece in self.0.iter() {
            match piece {
                Piece::Words(w) => F::escape(f, w)?,
                Piece::Markup(m) => m.fmt(f)?,
            }
        }
        Ok(())
    }
}

/// Something that can be added to a [`Text`] with markup `M`.
///
/// Words, numbers, quantities and lists are added for any `M`.  Implement it
/// for your own types, or wrap them in [`Plain`] or [`Markup`].
pub trait AddTo<M> {
    /// Adds this to the end of `text`.
    fn add_to(self, text: &mut Text<M>);
}

impl<M> AddTo<M> for Text<M> {
    fn add_to(self, text: &mut Text<M>) {
        for piece in self.0 {
            match piece {
                Piece::Words(w) => text.push_words(&w),
                Piece::Markup(m) => text.push_markup(m),
            }
        }
    }
}

impl<M, T: AddTo<M>> AddTo<M> for Option<T> {
    fn add_to(self, text: &mut Text<M>) {
        if let Some(t) = self {
            t.add_to(text);
        }
    }
}

/// A value added as the words its [`Display`] gives.
pub struct Plain<T>(pub T);

impl<M, T: Display> AddTo<M> for Plain<T> {
    fn add_to(self, text: &mut Text<M>) {
        text.push_words(&self.0.to_string());
    }
}

/// A value added as markup.
pub struct Markup<M>(pub M);

impl<M> AddTo<M> for Markup<M> {
    fn add_to(self, text: &mut Text<M>) {
        text.push_markup(self.0);
    }
}

macro_rules! add_as_words {
    ($($t:ty),*) => {
        $(impl<M> AddTo<M> for $t {
            fn add_to(self, text: &mut Text<M>) {
                text.push_words(&self.to_string());
            }
        })*
    };
}
add_as_words!(&str, String, &String, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
