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
/// Build a `Text` by adding pieces with `+` or `+=`, starting from a
/// [`Word`], a string followed by a `Word` or `Text`, markup that
/// [`markup!`](crate::markup!) was given, or `Text::from(...)`.  A piece is
/// anything that converts into a `Text`:
/// strings, numbers, [`Word`]s, `Option`s, other `Text`, and anything that
/// implements [`Markup`].  For a type of your own, implement `From<YourType>
/// for Text`.  `Display` shows it as plain text.  A `Text` is `Send`
/// and `Sync`, so a server can keep one in shared state or across an
/// `.await`.
///
/// Adding a verb makes a clause: the verb agrees with the subject of the
/// text it is added to, which is its first noun or noun phrase (see
/// [`number`](Text::number)), and plain words such as "partner" are
/// singular.  It agrees with the number that subject ends up with, so a
/// relative clause such as "that is high" follows the quantity it is later
/// counted by.  A text that starts with a verb is a predicate, whose verbs
/// all agree with the subject it is added to, so a relative clause on its
/// object, as in "has a card that is high", goes in [`a`](crate::a),
/// [`count`](crate::count) or parentheses.  `+` puts a space where letters or digits would otherwise
/// run together, counting a [`Word`] or markup as letters, and leaves
/// punctuation and spaces already there alone, so words can be added
/// directly:
///
/// ```
/// use wording::{numbered, Number, Word};
///
/// let partner = numbered("partner", Number::Singular);
/// let you = numbered("you", Number::Plural);
/// assert_eq!((partner.clone() + Word::HAS + "bid").to_string(), "partner has bid");
/// // A clause inside a clause goes in parentheses, so that its verb agrees
/// // with its own subject.
/// let knows = you + "know that" + (partner + Word::HAS + "bid");
/// assert_eq!(knows.to_string(), "you know that partner has bid");
/// ```
#[derive(Clone, Default)]
pub struct Text {
    pieces: Vec<Piece>,
}

#[derive(Clone)]
enum Piece {
    Words(String),
    /// A word, and what decides its number.
    Word(Word, Agreement),
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

/// What decides the number of a [`Word`].
#[derive(Clone, Copy)]
enum Agreement {
    /// Nothing yet, so it is singular until something settles it.
    Free,
    /// Settled.
    Settled(Number),
    /// A verb that agrees with the subject this many pieces before it.
    Subject(usize),
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
/// use wording::{Form, Markup, Text};
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
/// let t = Text::from("Partner opened ") + Notrump(1) + ".";
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
        self.pieces.push(Piece::Word(word, Agreement::Free));
    }
    pub(crate) fn push_article(&mut self) {
        self.pieces.push(Piece::Article);
    }
    /// Is there nothing to display?
    pub fn is_empty(&self) -> bool {
        self.pieces.iter().all(is_marker)
    }
    /// Whether this is one or many as the subject of a verb.
    ///
    /// That is the number of the first noun or noun phrase in it, so "the
    /// card with 5 points" is singular and "your 7 points" plural.  A noun
    /// phrase is a quantity, a list, or a phrase given to [`a`](crate::a) or
    /// [`numbered`](crate::numbered).  Text with neither is singular, as for
    /// "partner".
    pub fn number(&self) -> Number {
        self.subject()
            .map_or(Number::Singular, |i| self.number_at(i))
    }
    /// Where the subject is: the first noun or noun phrase.
    fn subject(&self) -> Option<usize> {
        self.pieces.iter().position(|p| match p {
            Piece::Number(_) => true,
            Piece::Word(w, _) => w.is_noun(),
            Piece::Markup(m, _) => m.noun().is_some(),
            _ => false,
        })
    }
    /// The number of the piece at `i`.
    fn number_at(&self, i: usize) -> Number {
        match &self.pieces[i] {
            Piece::Number(n) => *n,
            Piece::Word(_, Agreement::Settled(n)) => *n,
            Piece::Word(_, Agreement::Subject(back)) => self.number_at(i - back),
            Piece::Markup(m, n) => n.or(m.noun()).unwrap_or(Number::Singular),
            _ => Number::Singular,
        }
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
        for piece in self.pieces.iter_mut() {
            if let Piece::Word(w, a @ Agreement::Free) = piece {
                if !w.is_noun() {
                    *a = Agreement::Settled(number);
                }
            }
        }
        for piece in self.pieces.iter_mut() {
            match piece {
                Piece::Word(w, a @ Agreement::Free) if w.is_noun() => {
                    *a = Agreement::Settled(number);
                    break;
                }
                Piece::Markup(m, n @ None) if m.noun().is_some() => {
                    *n = Some(number);
                    break;
                }
                _ => (),
            }
        }
        self.mark_number(number);
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

    fn write<F: MarkupFormat>(&self, f: &mut Formatter) -> fmt::Result {
        let mut next = Next::default();
        for (i, piece) in self.pieces.iter().enumerate() {
            match piece {
                Piece::Number(_) => {}
                Piece::Capital => next.capital = true,
                Piece::Article => next.article = true,
                Piece::Words(w) => next.write_words::<F>(w, f)?,
                Piece::Word(word, _) => next.write_words::<F>(word.form(self.number_at(i)), f)?,
                Piece::Markup(m, n) => {
                    let number = n.or(m.noun()).unwrap_or(Number::Singular);
                    if std::mem::take(&mut next.article) {
                        let form = Form {
                            number,
                            capital: false,
                        };
                        let article = if m.vowel_sound(form) { "an " } else { "a " };
                        next.write_words::<F>(article, f)?;
                    }
                    let form = Form {
                        number,
                        capital: std::mem::take(&mut next.capital),
                    };
                    F::markup(m.as_ref(), form, f)?;
                }
            }
        }
        Ok(())
    }
}

/// A format that a `Text` displays in, and that [`Markup`] has a method for.
trait MarkupFormat: Format {
    fn markup(markup: &dyn Markup, form: Form, f: &mut Formatter) -> fmt::Result;
}

impl MarkupFormat for HTML {
    fn markup(markup: &dyn Markup, form: Form, f: &mut Formatter) -> fmt::Result {
        markup.html(form, f)
    }
}

impl MarkupFormat for UTF8 {
    fn markup(markup: &dyn Markup, form: Form, f: &mut Formatter) -> fmt::Result {
        markup.utf8(form, f)
    }
}

impl MarkupFormat for LaTeX {
    fn markup(markup: &dyn Markup, form: Form, f: &mut Formatter) -> fmt::Result {
        markup.latex(form, f)
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
    fn write_words<F: Format>(&mut self, words: &str, f: &mut Formatter) -> fmt::Result {
        let Some(start) = words.find(char::is_alphanumeric) else {
            return F::escape(f, words);
        };
        if !self.capital && !self.article {
            return F::escape(f, words);
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
        F::escape(f, before)?;
        F::escape(f, &shown)
    }
}

impl DisplayAs<HTML> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write::<HTML>(f)
    }
}

impl DisplayAs<UTF8> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write::<UTF8>(f)
    }
}

impl DisplayAs<LaTeX> for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write::<LaTeX>(f)
    }
}

/// The text as plain text.
impl Display for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write::<UTF8>(f)
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Text").field(&self.to_string()).finish()
    }
}

impl Text {
    /// Adds `other` to the end as it is, keeping what decides the number of
    /// each of its words.
    pub(crate) fn append(&mut self, other: Text) {
        self.join(other, FreeVerbs::Stay);
    }
    /// Adds `other` to the end as `+` does: with a space where words would
    /// run together, and with each verb of `other` whose number is free
    /// agreeing with this text's subject.
    fn add_clause(&mut self, other: Text) {
        if self.needs_space_before(&other) {
            self.push_words(" ");
        }
        let verbs = self.free_verbs_agree_with();
        self.join(other, verbs);
    }
    /// What the free verbs of a text added with `+` agree with: this text's
    /// subject, or the singular when it has none, as for "partner".  They
    /// stay free when nothing is shown yet, or when this text starts with a
    /// free verb, since then it is a predicate whose subject is still to come
    /// (a noun after that verb is its object, not the subject).
    fn free_verbs_agree_with(&self) -> FreeVerbs {
        match (self.first_shown(), self.subject()) {
            (None, _) => FreeVerbs::Stay,
            (Some(Piece::Word(w, Agreement::Free)), _) if !w.is_noun() => FreeVerbs::Stay,
            (_, Some(subject)) => FreeVerbs::Subject(subject),
            (_, None) => FreeVerbs::Singular,
        }
    }
    /// Adds the pieces of `other`, settling its free verbs as `verbs` says.
    fn join(&mut self, other: Text, verbs: FreeVerbs) {
        for piece in other.pieces {
            match (piece, verbs) {
                (Piece::Words(w), _) => self.push_words(&w),
                (Piece::Word(w, Agreement::Free), FreeVerbs::Subject(at)) if !w.is_noun() => {
                    let back = self.pieces.len() - at;
                    self.pieces.push(Piece::Word(w, Agreement::Subject(back)));
                }
                (Piece::Word(w, Agreement::Free), FreeVerbs::Singular) if !w.is_noun() => {
                    let singular = Agreement::Settled(Number::Singular);
                    self.pieces.push(Piece::Word(w, singular));
                }
                (piece, _) => self.pieces.push(piece),
            }
        }
    }
    /// The first piece that shows something other than spaces.
    fn first_shown(&self) -> Option<&Piece> {
        self.pieces.iter().find(|p| shows(p))
    }
    /// Does a space go between this and `other`?  It does where letters or
    /// digits would otherwise run together, counting a [`Word`] or markup as
    /// letters, so punctuation and spaces already there are left alone, except
    /// that a word goes after a comma, a full stop or a closing bracket with
    /// a space.  Quotes are left alone, since an opening quote takes none.
    fn needs_space_before(&self, other: &Text) -> bool {
        let left = match self.pieces.iter().rev().find(|p| !is_marker(p)) {
            Some(Piece::Words(w)) => {
                w.ends_with(char::is_alphanumeric)
                    || w.ends_with([',', ';', ':', '.', '?', '!', ')', ']'])
            }
            Some(_) => true,
            None => false,
        };
        let right = match other
            .pieces
            .iter()
            .find(|p| !is_marker(p) || matches!(p, Piece::Article))
        {
            Some(Piece::Words(w)) => w.starts_with(char::is_alphanumeric),
            Some(_) => true,
            None => false,
        };
        left && right
    }
}

/// What happens to verbs whose number is free when a text is added.
#[derive(Clone, Copy)]
enum FreeVerbs {
    /// They stay free.
    Stay,
    /// They agree with the subject at this index.
    Subject(usize),
    /// They are singular.
    Singular,
}

/// Is this piece only a marker, showing nothing itself?
fn is_marker(piece: &Piece) -> bool {
    match piece {
        Piece::Words(w) => w.is_empty(),
        Piece::Number(_) | Piece::Capital | Piece::Article => true,
        Piece::Word(..) | Piece::Markup(..) => false,
    }
}

/// Does this piece show something other than spaces?
fn shows(piece: &Piece) -> bool {
    match piece {
        Piece::Words(w) => !w.trim().is_empty(),
        Piece::Word(..) | Piece::Markup(..) => true,
        Piece::Number(_) | Piece::Capital | Piece::Article => false,
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

/// `"words" + word` starts a text with words, as in `"partner" + Word::HAS`.
impl Add<Word> for &str {
    type Output = Text;
    fn add(self, word: Word) -> Text {
        Text::from(self) + word
    }
}

/// `"words" + text` starts a text with words, escaped for each format:
///
/// ```
/// use display_as::{format_as, HTML};
/// use wording::Text;
///
/// let t = "Made by" + Text::from("Smith & Wesson") + ".";
/// assert_eq!(format_as!(HTML, t).into_string(), "Made by Smith &amp; Wesson.");
/// ```
impl Add<Text> for &str {
    type Output = Text;
    fn add(self, text: Text) -> Text {
        Text::from(self) + text
    }
}

/// `text += piece` adds anything that converts into a `Text`.
impl<T: Into<Text>> AddAssign<T> for Text {
    fn add_assign(&mut self, piece: T) {
        self.add_clause(piece.into());
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
