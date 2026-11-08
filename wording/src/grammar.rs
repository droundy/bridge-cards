use crate::{text, AddTo, NoMarkup, Text};
use std::fmt::{self, Display, Formatter};

/// Whether something is one or many, which nouns and verbs agree with.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Number {
    /// One.
    Singular,
    /// None, or more than one.
    Plural,
}

impl Number {
    /// The number for a count of `n`: singular only for one.
    pub fn of(n: u64) -> Self {
        if n == 1 {
            Number::Singular
        } else {
            Number::Plural
        }
    }
}

/// A word with a singular and a plural form: a noun such as "point" and
/// "points", or a verb such as "is" and "are".
///
/// It agrees with whatever number a [`Quantity`] or [`agree`] settles for
/// it, and is singular until then.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Word {
    singular: &'static str,
    plural: &'static str,
}

/// "is" or "are".
pub const IS: Word = Word::new("is", "are");
/// "has" or "have".
pub const HAS: Word = Word::new("has", "have");

impl Word {
    /// A word such as `Word::new("spade", "spades")` or `Word::new("shows",
    /// "show")`.  A verb's singular is the form that goes with "it".
    pub const fn new(singular: &'static str, plural: &'static str) -> Self {
        Word { singular, plural }
    }
}

impl<M> AddTo<M> for Word {
    fn add_to(self, text: &mut Text<M>) {
        text.push_word(self.singular, self.plural);
    }
}

/// Something a verb can agree with, because it knows whether it is one or
/// many: a [`Quantity`], a [`List`](crate::List) of subjects, or a phrase
/// given its number with [`numbered`].
pub trait Subject<M>: AddTo<M> {
    /// Whether this is one or many.
    fn number(&self) -> Number;
}

/// `predicate` agreeing with `subject`, as in "partner has 5+ spades" or
/// "you have 5+ spades".
///
/// Only the words in `predicate` whose number is not already settled
/// agree, so "5+ spades" stays plural after "partner has".
pub fn agree<M>(subject: impl Subject<M>, predicate: impl AddTo<M>) -> Text<M> {
    let number = subject.number();
    text!(subject, " ", text!(predicate).agreeing(number))
}

/// A phrase whose number is given by hand, so that a verb can agree with
/// it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Numbered<T> {
    phrase: T,
    number: Number,
}

/// `phrase`, which is one or many as `number` says, as in
/// `numbered("partner", Number::Singular)` or `numbered("you",
/// Number::Plural)`.  Any [`Word`] in it agrees with `number`.
pub fn numbered<T>(phrase: T, number: Number) -> Numbered<T> {
    Numbered { phrase, number }
}

impl<M, T: AddTo<M>> AddTo<M> for Numbered<T> {
    fn add_to(self, text: &mut Text<M>) {
        text!(self.phrase).agreeing(self.number).add_to(text);
    }
}

impl<M, T: AddTo<M>> Subject<M> for Numbered<T> {
    fn number(&self) -> Number {
        self.number
    }
}

/// An amount of a noun, as in "1 spade", "5+ spades" or "6–9 high card
/// points".
///
/// The noun is any phrase, and its words and markup agree with the amount,
/// so "at most 1 spade" is singular and "1+ spades" plural.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Quantity<M = NoMarkup> {
    amount: Amount,
    noun: Text<M>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Amount {
    Exactly(u64),
    AtLeast(u64),
    AtMost(u64),
    Between(u64, u64),
    Other(String, Number),
}

impl Amount {
    fn number(&self) -> Number {
        match *self {
            Amount::Exactly(n) | Amount::AtMost(n) => Number::of(n),
            Amount::AtLeast(_) | Amount::Between(..) => Number::Plural,
            Amount::Other(_, number) => number,
        }
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Amount::Exactly(n) => write!(f, "{n}"),
            Amount::AtLeast(n) => write!(f, "{n}+"),
            Amount::AtMost(n) => write!(f, "at most {n}"),
            Amount::Between(lo, hi) => write!(f, "{lo}–{hi}"),
            Amount::Other(amount, _) => f.write_str(amount),
        }
    }
}

/// `n` of `noun`, as in "1 spade" or "7 spades".
pub fn count<M>(n: impl Into<u64>, noun: impl AddTo<M>) -> Quantity<M> {
    Quantity::new(Amount::Exactly(n.into()), noun)
}

/// `n` or more of `noun`, as in "5+ spades".
pub fn at_least<M>(n: impl Into<u64>, noun: impl AddTo<M>) -> Quantity<M> {
    Quantity::new(Amount::AtLeast(n.into()), noun)
}

/// `n` or fewer of `noun`, as in "at most 1 spade".
pub fn at_most<M>(n: impl Into<u64>, noun: impl AddTo<M>) -> Quantity<M> {
    Quantity::new(Amount::AtMost(n.into()), noun)
}

/// From `lo` to `hi` of `noun`, as in "6–9 points".
pub fn between<M>(lo: impl Into<u64>, hi: impl Into<u64>, noun: impl AddTo<M>) -> Quantity<M> {
    Quantity::new(Amount::Between(lo.into(), hi.into()), noun)
}

impl<M> Quantity<M> {
    fn new(amount: Amount, noun: impl AddTo<M>) -> Self {
        Quantity {
            amount,
            noun: text!(noun),
        }
    }
    /// An amount the other constructors can't write, as in "6½ losers".
    /// The noun agrees with `number`.
    pub fn other(amount: impl Display, number: Number, noun: impl AddTo<M>) -> Self {
        Quantity::new(Amount::Other(amount.to_string(), number), noun)
    }
}

impl<M> AddTo<M> for Quantity<M> {
    fn add_to(self, text: &mut Text<M>) {
        let number = self.amount.number();
        text.push_words(&format!("{} ", self.amount));
        self.noun.agreeing(number).add_to(text);
    }
}

impl<M> Subject<M> for Quantity<M> {
    fn number(&self) -> Number {
        self.amount.number()
    }
}

/// A phrase with "a" or "an" in front of it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Indefinite<T>(T);

/// `phrase` with "a" or "an" in front, as in "an ace" or "a king".
///
/// The choice goes by the first letter, with exceptions for the usual words
/// that start with a silent "h" ("an hour") or a "y" or "w" sound ("a one",
/// "a unit", "a eulogy").  Other exceptions need the article written out.
pub fn a<T: Display>(phrase: T) -> Indefinite<T> {
    Indefinite(phrase)
}

impl<T: Display> Display for Indefinite<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let phrase = self.0.to_string();
        write!(f, "{} {}", article(&phrase), phrase)
    }
}

impl<M, T: Display> AddTo<M> for Indefinite<T> {
    fn add_to(self, text: &mut Text<M>) {
        text.push_words(&self.to_string());
    }
}

/// "a" or "an", whichever goes before `word`.
fn article(word: &str) -> &'static str {
    let lower = word.to_lowercase();
    const SILENT_H: [&str; 4] = ["hour", "honest", "honour", "heir"];
    const CONSONANT_SOUND: [&str; 6] = ["one", "once", "uni", "use", "usu", "eu"];
    if SILENT_H.iter().any(|p| lower.starts_with(p)) {
        "an"
    } else if CONSONANT_SOUND.iter().any(|p| lower.starts_with(p)) {
        "a"
    } else if lower.starts_with(['a', 'e', 'i', 'o', 'u']) {
        "an"
    } else {
        "a"
    }
}
