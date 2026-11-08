use crate::Number;
use display_as::{DisplayAs, Format};
use std::fmt::{Display, Error, Formatter};
use std::ops::{Add, AddAssign};

/// Words and markup, displayed in any format the markup supports.
///
/// Words are escaped for the format they are displayed in.  Markup is a
/// value of type `M`, such as a playing card that HTML shows in colour, and
/// displays itself through [`DisplayAs`].  A [`Word`](crate::Word) and a
/// piece of markup agree with whatever number a [`Quantity`](crate::Quantity)
/// or [`agree`](crate::agree) settles for them, and are singular until then.
///
/// Build a `Text` with [`text!`](crate::text!), or by adding pieces to one
/// with `+` or `+=`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Text<M = NoMarkup>(Vec<Piece<M>>);

#[derive(Clone, Debug, PartialEq, Eq)]
enum Piece<M> {
    Words(String),
    /// A word with a plural, and the number it agrees with once that is
    /// settled.
    Word {
        singular: String,
        plural: String,
        number: Option<Number>,
    },
    Markup(M, Option<Number>),
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

/// The format `F`, for markup that agrees with a plural.
///
/// Markup displays in `F` when it is singular and in `Plural<F>` when it is
/// plural, so a suit might be "spade" and "spades" in plain text but "♠" in
/// HTML either way.  Markup that never changes can display the same in
/// both:
///
/// ```
/// use display_as::{format_as, DisplayAs, Format, HTML, UTF8};
/// use std::fmt::{Error, Formatter};
/// use wording::{count, Plural, Text};
///
/// struct Bid(u8);
/// # impl DisplayAs<UTF8> for Bid {
/// #     fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
/// #         write!(f, "{}NT", self.0)
/// #     }
/// # }
/// # impl DisplayAs<HTML> for Bid {
/// #     fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
/// #         write!(f, "<b>{}NT</b>", self.0)
/// #     }
/// # }
/// # impl wording::AddTo<Bid> for Bid {
/// #     fn add_to(self, text: &mut Text<Bid>) { text.push_markup(self) }
/// # }
/// impl<F: Format> DisplayAs<Plural<F>> for Bid
/// where
///     Bid: DisplayAs<F>,
/// {
///     fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
///         <Bid as DisplayAs<F>>::fmt(self, f)
///     }
/// }
///
/// let bids: Text<Bid> = wording::text!(count(2u8, Bid(3)));
/// assert_eq!(format_as!(HTML, bids).into_string(), "2 <b>3NT</b>");
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Plural<F>(F);

impl<F: Format> Format for Plural<F> {
    fn escape(f: &mut Formatter, s: &str) -> Result<(), Error> {
        F::escape(f, s)
    }
    fn mime() -> mime::Mime {
        F::mime()
    }
    fn this_format() -> Self {
        Plural(F::this_format())
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
        self.0.push(Piece::Markup(markup, None));
    }
    pub(crate) fn push_word(&mut self, singular: &str, plural: &str) {
        self.0.push(Piece::Word {
            singular: singular.to_string(),
            plural: plural.to_string(),
            number: None,
        });
    }
    /// The text with every word and piece of markup whose number is not yet
    /// settled agreeing with `number`.
    pub fn agreeing(mut self, number: Number) -> Self {
        for piece in self.0.iter_mut() {
            if let Piece::Word { number: n, .. } | Piece::Markup(_, n) = piece {
                n.get_or_insert(number);
            }
        }
        self
    }
    /// Is there nothing to display?
    pub fn is_empty(&self) -> bool {
        self.0
            .iter()
            .all(|p| matches!(p, Piece::Words(w) if w.is_empty()))
    }
    /// The text with a capital first letter, if it starts with words.
    pub fn capitalized(mut self) -> Self {
        match self.0.first_mut() {
            Some(Piece::Words(w)) => capitalize(w),
            Some(Piece::Word {
                singular, plural, ..
            }) => {
                capitalize(singular);
                capitalize(plural);
            }
            _ => (),
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

fn capitalize(w: &mut String) {
    let mut chars = w.chars();
    if let Some(c) = chars.next() {
        *w = c.to_uppercase().chain(chars).collect();
    }
}

impl<F: Format, M: DisplayAs<F> + DisplayAs<Plural<F>>> DisplayAs<F> for Text<M> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for piece in self.0.iter() {
            match piece {
                Piece::Words(w) => F::escape(f, w)?,
                Piece::Word {
                    plural,
                    number: Some(Number::Plural),
                    ..
                } => F::escape(f, plural)?,
                Piece::Word { singular, .. } => F::escape(f, singular)?,
                Piece::Markup(m, Some(Number::Plural)) => <M as DisplayAs<Plural<F>>>::fmt(m, f)?,
                Piece::Markup(m, _) => <M as DisplayAs<F>>::fmt(m, f)?,
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
                piece => text.0.push(piece),
            }
        }
    }
}

/// `text + piece` adds anything that implements [`AddTo`].
impl<M, T: AddTo<M>> Add<T> for Text<M> {
    type Output = Text<M>;
    fn add(mut self, piece: T) -> Text<M> {
        piece.add_to(&mut self);
        self
    }
}

/// `text += piece` adds anything that implements [`AddTo`].
impl<M, T: AddTo<M>> AddAssign<T> for Text<M> {
    fn add_assign(&mut self, piece: T) {
        piece.add_to(self);
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
