use crate::grammar::vowel_sound;
use crate::{Number, Word};
use display_as::{DisplayAs, Format, LaTeX, HTML, UTF8};
use std::fmt::{self, Display, Formatter};
use std::ops::{Add, AddAssign};
use std::sync::Arc;

/// A phrase: words and markup, displayed as HTML, plain text or LaTeX.
///
/// Every piece of wording is a `Text`, so any phrase can go inside any other:
/// a quantity can be the object of one sentence and the subject of the next.
/// Words are escaped for the format they are displayed in, and [`Markup`],
/// such as a playing card that HTML shows in colour, displays itself.
///
/// Build a `Text` with [`text!`](crate::text!), or by adding pieces to one
/// with `+` or `+=`.  A piece is anything that converts into a `Text`:
/// strings, numbers, [`Word`]s, `Option`s, other `Text`, and anything that
/// implements [`Markup`].  For a type of your own, implement `From<YourType>
/// for Text`.  `Display` shows it as plain text.  A `Text` is `Send`
/// and `Sync`, so a server can keep one in shared state or across an
/// `.await`.
#[derive(Clone, Default)]
pub struct Text {
    pieces: Vec<Piece>,
}

#[derive(Clone)]
enum Piece {
    Words(String),
    /// A word, and the number it agrees with once that is settled.
    Word(Word, Option<Number>),
    /// Markup, and the number it agrees with once that is settled.
    Markup(Arc<dyn Markup>, Option<Number>),
    /// The number, as a subject, of the phrase that starts here: a quantity,
    /// a list, or a phrase given to [`a`](crate::a) or
    /// [`numbered`](crate::numbered).
    Number(Number),
    /// The next letter shown is a capital.
    Capital,
    /// "a" or "an", whichever goes before what is shown next.
    Article,
}

/// Something that displays itself differently in each format, such as a bid,
/// or a suit that plain text spells out and HTML shows as a symbol.
///
/// Anything that implements `Markup` converts into a [`Text`].  Markup is
/// `Send` and `Sync`, as plain data such as a bid or a suit is, so that a
/// `Text` is too:
///
/// ```
/// use display_as::{format_as, HTML};
/// use std::fmt::{self, Formatter};
/// use wording::{text, Form, Markup};
///
/// struct Notrump(u8);
///
/// impl Markup for Notrump {
///     fn html(&self, _: Form, f: &mut Formatter) -> fmt::Result {
///         write!(f, "<b>{}NT</b>", self.0)
///     }
///     fn utf8(&self, _: Form, f: &mut Formatter) -> fmt::Result {
///         write!(f, "{}NT", self.0)
///     }
///     fn latex(&self, _: Form, f: &mut Formatter) -> fmt::Result {
///         write!(f, r"\textbf{{{}NT}}", self.0)
///     }
/// }
///
/// let t = text!("Partner opened ", Notrump(1), ".");
/// assert_eq!(t.to_string(), "Partner opened 1NT.");
/// assert_eq!(format_as!(HTML, t).into_string(), "Partner opened <b>1NT</b>.");
/// ```
pub trait Markup: Send + Sync {
    /// Writes this as HTML.
    fn html(&self, form: Form, f: &mut Formatter) -> fmt::Result;
    /// Writes this as plain text.
    fn utf8(&self, form: Form, f: &mut Formatter) -> fmt::Result;
    /// Writes this as LaTeX.
    fn latex(&self, form: Form, f: &mut Formatter) -> fmt::Result;
    /// If this is a noun, the number it takes when nothing settles one.
    ///
    /// A noun agrees with a quantity it is the noun of, as in "1 spade", and
    /// decides the number of a subject it is the first noun of, as in "spades
    /// are trumps".  Markup that is not a noun, such as a bid, keeps the
    /// default `None` and is always given [`Number::Singular`].
    fn noun(&self) -> Option<Number> {
        None
    }
    /// Does this start with a vowel sound, so that "an" goes before it?  By
    /// default this goes by how it is written as plain text.
    fn vowel_sound(&self, form: Form) -> bool {
        struct Shown<'a, M: ?Sized>(&'a M, Form);
        impl<M: Markup + ?Sized> Display for Shown<'_, M> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                self.0.utf8(self.1, f)
            }
        }
        vowel_sound(&Shown(self, form).to_string())
    }
}

/// How [`Markup`] is to be written.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct Form {
    /// Singular or plural, for markup that is a noun.  Other markup is always
    /// singular.
    pub number: Number,
    /// Does this start a sentence, so that any words in it start with a
    /// capital?
    pub capital: bool,
}

/// Text made of words, quantities, lists and markup: anything that
/// converts into a [`Text`].
///
/// ```
/// use display_as::{format_as, HTML};
/// use wording::text;
///
/// let name = "Smith & Wesson";
/// let t = text!("Made by ", name, ".");
/// assert_eq!(format_as!(HTML, t).into_string(), "Made by Smith &amp; Wesson.");
/// ```
#[macro_export]
macro_rules! text {
    ($($piece:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut text = $crate::Text::new();
        $( text += $piece; )*
        text
    }};
}

impl Text {
    /// Empty text.
    pub fn new() -> Self {
        Text::default()
    }
    /// Adds words to the end.
    pub(crate) fn push_words(&mut self, words: &str) {
        match self.pieces.last_mut() {
            Some(Piece::Words(w)) => w.push_str(words),
            _ => self.pieces.push(Piece::Words(words.to_string())),
        }
    }
    pub(crate) fn push_word(&mut self, word: Word) {
        self.pieces.push(Piece::Word(word, None));
    }
    pub(crate) fn push_article(&mut self) {
        self.pieces.push(Piece::Article);
    }
    /// Is there nothing to display?
    pub fn is_empty(&self) -> bool {
        self.pieces.iter().all(|p| match p {
            Piece::Words(w) => w.is_empty(),
            Piece::Number(_) | Piece::Capital | Piece::Article => true,
            Piece::Word(..) | Piece::Markup(..) => false,
        })
    }
    /// Whether this is one or many as the subject of a verb.
    ///
    /// That is the number of the first noun or noun phrase in it, so "the
    /// card with 5 points" is singular and "your 7 points" plural.  A noun
    /// phrase is a quantity, a list, or a phrase given to [`a`](crate::a) or
    /// [`numbered`](crate::numbered).  Text with neither is singular, as for
    /// "partner".
    pub fn number(&self) -> Number {
        self.pieces
            .iter()
            .find_map(|p| match p {
                Piece::Number(n) => Some(*n),
                Piece::Word(w, n) if w.is_noun() => Some(n.unwrap_or(Number::Singular)),
                Piece::Markup(m, n) => m.noun().map(|default| n.unwrap_or(default)),
                _ => None,
            })
            .unwrap_or(Number::Singular)
    }
    /// Makes this a phrase that is one or many as `number` says, as a
    /// subject, without changing the words in it.
    pub(crate) fn mark_number(&mut self, number: Number) {
        self.pieces.insert(0, Piece::Number(number));
    }
    /// Makes this a noun phrase that is one or many as `number` says.  Its
    /// head, the first noun whose number is not yet settled, agrees with
    /// `number`, as in "1 card in hearts", and so do the verbs whose number
    /// is not yet settled, as in "5 cards that are high", so that the phrase
    /// reads the same in any sentence.
    pub(crate) fn settle(&mut self, number: Number) {
        self.settle_verbs(number);
        let head = self.pieces.iter_mut().find_map(|p| match p {
            Piece::Word(w, n @ None) if w.is_noun() => Some(n),
            Piece::Markup(m, n @ None) if m.noun().is_some() => Some(n),
            _ => None,
        });
        if let Some(n) = head {
            *n = Some(number);
        }
        self.mark_number(number);
    }
    /// Makes every verb whose number is not yet settled agree with `number`.
    pub(crate) fn settle_verbs(&mut self, number: Number) {
        for piece in self.pieces.iter_mut() {
            if let Piece::Word(w, n @ None) = piece {
                if !w.is_noun() {
                    *n = Some(number);
                }
            }
        }
    }
    /// The text starting with a capital letter.
    ///
    /// The capital goes on the first letter displayed, so that it works after
    /// an empty phrase, and markup is told to start with a capital.
    pub fn capitalized(mut self) -> Self {
        self.pieces.insert(0, Piece::Capital);
        self
    }
    /// The text as a sentence: capitalized, and ending in a full stop unless
    /// it already ends in one, a question mark or an exclamation mark (inside
    /// any closing quotes or brackets).  Trailing spaces are dropped.
    pub fn sentence(mut self) -> Self {
        if let Some(Piece::Words(w)) = self.pieces.last_mut() {
            w.truncate(w.trim_end().len());
        }
        let ended = match self.pieces.last() {
            Some(Piece::Words(w)) => w
                .trim_end_matches(['"', '\'', '”', '’', ')', ']'])
                .ends_with(['.', '?', '!']),
            _ => false,
        };
        if !ended {
            self.push_words(".");
        }
        self.capitalized()
    }

    fn write(&self, out: Out, f: &mut Formatter) -> fmt::Result {
        let mut next = Next::default();
        for piece in self.pieces.iter() {
            match piece {
                Piece::Number(_) => {}
                Piece::Capital => next.capital = true,
                Piece::Article => next.article = true,
                Piece::Words(w) => next.write_words(out, w, f)?,
                Piece::Word(word, n) => {
                    next.write_words(out, word.form(n.unwrap_or(Number::Singular)), f)?
                }
                Piece::Markup(m, n) => {
                    let number = n.or(m.noun()).unwrap_or(Number::Singular);
                    if std::mem::take(&mut next.article) {
                        let form = Form {
                            number,
                            capital: false,
                        };
                        let article = if m.vowel_sound(form) { "an " } else { "a " };
                        next.write_words(out, article, f)?;
                    }
                    let form = Form {
                        number,
                        capital: std::mem::take(&mut next.capital),
                    };
                    match out {
                        Out::Html => m.html(form, f)?,
                        Out::Utf8 => m.utf8(form, f)?,
                        Out::Latex => m.latex(form, f)?,
                    }
                }
            }
        }
        Ok(())
    }
}

/// The formats a `Text` displays in.
#[derive(Clone, Copy)]
enum Out {
    Html,
    Utf8,
    Latex,
}

impl Out {
    fn escape(self, f: &mut Formatter, s: &str) -> fmt::Result {
        match self {
            Out::Html => HTML::escape(f, s),
            Out::Utf8 => UTF8::escape(f, s),
            Out::Latex => LaTeX::escape(f, s),
        }
    }
}

/// What has to happen to the next letter displayed.
#[derive(Default)]
struct Next {
    capital: bool,
    article: bool,
}

impl Next {
    /// Writes `words`, putting any pending article and capital on its first
    /// letter or digit.  Words with neither, such as "(", leave them pending.
    fn write_words(&mut self, out: Out, words: &str, f: &mut Formatter) -> fmt::Result {
        let Some(start) = words.find(char::is_alphanumeric) else {
            return out.escape(f, words);
        };
        if !self.capital && !self.article {
            return out.escape(f, words);
        }
        let (before, rest) = words.split_at(start);
        let mut shown = String::new();
        if std::mem::take(&mut self.article) {
            shown.push_str(if vowel_sound(rest) { "an " } else { "a " });
        }
        shown.push_str(rest);
        if std::mem::take(&mut self.capital) {
            let mut chars = shown.chars();
            if let Some(c) = chars.next() {
                shown = c.to_uppercase().chain(chars).collect();
            }
        }
        out.escape(f, before)?;
        out.escape(f, &shown)
    }
}

impl DisplayAs<HTML> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write(Out::Html, f)
    }
}

impl DisplayAs<UTF8> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write(Out::Utf8, f)
    }
}

impl DisplayAs<LaTeX> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write(Out::Latex, f)
    }
}

/// The text as plain text.
impl Display for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write(Out::Utf8, f)
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Text").field(&self.to_string()).finish()
    }
}

impl Text {
    /// Adds `other` to the end, keeping the numbers its words have settled.
    pub(crate) fn append(&mut self, other: Text) {
        for piece in other.pieces {
            match piece {
                Piece::Words(w) => self.push_words(&w),
                piece => self.pieces.push(piece),
            }
        }
    }
}

/// `text + piece` adds anything that converts into a `Text`.
impl<T: Into<Text>> Add<T> for Text {
    type Output = Text;
    fn add(mut self, piece: T) -> Text {
        self += piece;
        self
    }
}

/// `text += piece` adds anything that converts into a `Text`.
impl<T: Into<Text>> AddAssign<T> for Text {
    fn add_assign(&mut self, piece: T) {
        self.append(piece.into());
    }
}

/// Markup as a `Text`, so that it can go anywhere a phrase can.
impl<T: Markup + 'static> From<T> for Text {
    fn from(markup: T) -> Self {
        Text {
            pieces: vec![Piece::Markup(Arc::new(markup), None)],
        }
    }
}

/// Nothing, or the phrase.
impl<T: Into<Text>> From<Option<T>> for Text {
    fn from(piece: Option<T>) -> Self {
        piece.map_or_else(Text::new, Into::into)
    }
}

macro_rules! from_words {
    ($($t:ty),*) => {
        $(impl From<$t> for Text {
            fn from(words: $t) -> Self {
                let mut text = Text::new();
                text.push_words(&words.to_string());
                text
            }
        })*
    };
}
from_words!(&str, String, &String, char, u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
