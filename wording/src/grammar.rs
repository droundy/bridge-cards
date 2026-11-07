use crate::{AddTo, Text};
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

/// A noun, with its singular and plural forms.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Noun {
    singular: &'static str,
    plural: &'static str,
}

impl Noun {
    /// A noun such as `Noun::new("spade", "spades")`.
    pub const fn new(singular: &'static str, plural: &'static str) -> Self {
        Noun { singular, plural }
    }
    /// The form that agrees with `number`.
    pub fn agreeing(self, number: Number) -> &'static str {
        match number {
            Number::Singular => self.singular,
            Number::Plural => self.plural,
        }
    }
}

/// A verb, with the forms that agree with a singular subject in the third
/// person and with any other subject.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Verb {
    singular: &'static str,
    plural: &'static str,
}

/// "is" or "are".
pub const IS: Verb = Verb::new("is", "are");
/// "has" or "have".
pub const HAS: Verb = Verb::new("has", "have");

impl Verb {
    /// A verb such as `Verb::new("shows", "show")`.
    pub const fn new(singular: &'static str, plural: &'static str) -> Self {
        Verb { singular, plural }
    }
    /// The form that agrees with a subject of `number` in the third person.
    /// "You" takes the plural form.
    pub fn agreeing(self, number: Number) -> &'static str {
        match number {
            Number::Singular => self.singular,
            Number::Plural => self.plural,
        }
    }
}

/// Something that can be counted, such as a [`Noun`].
///
/// Implement it for markup that should agree with a number, such as a suit
/// that plain text spells "spade" or "spades" and HTML shows as "♠".
pub trait Countable<M> {
    /// Adds the form that agrees with `number` to the end of `text`.
    fn add_agreeing(self, number: Number, text: &mut Text<M>);
}

impl<M> Countable<M> for Noun {
    fn add_agreeing(self, number: Number, text: &mut Text<M>) {
        text.push_words(self.agreeing(number));
    }
}

/// An amount of something [`Countable`], as in "1 spade", "5+ spades" or
/// "6–9 points".
///
/// The noun agrees with the amount, so "at most 1 spade" is singular and
/// "1+ spades" plural.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Quantity<N = Noun> {
    amount: Amount,
    noun: N,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Amount {
    Exactly(u64),
    AtLeast(u64),
    AtMost(u64),
    Between(u64, u64),
    Other(String, Number),
}

/// `n` of `noun`, as in "1 spade" or "7 spades".
pub fn count<N>(n: impl Into<u64>, noun: N) -> Quantity<N> {
    Quantity::new(Amount::Exactly(n.into()), noun)
}

/// `n` or more of `noun`, as in "5+ spades".
pub fn at_least<N>(n: impl Into<u64>, noun: N) -> Quantity<N> {
    Quantity::new(Amount::AtLeast(n.into()), noun)
}

/// `n` or fewer of `noun`, as in "at most 1 spade".
pub fn at_most<N>(n: impl Into<u64>, noun: N) -> Quantity<N> {
    Quantity::new(Amount::AtMost(n.into()), noun)
}

/// From `lo` to `hi` of `noun`, as in "6–9 points".
pub fn between<N>(lo: impl Into<u64>, hi: impl Into<u64>, noun: N) -> Quantity<N> {
    Quantity::new(Amount::Between(lo.into(), hi.into()), noun)
}

impl<N> Quantity<N> {
    fn new(amount: Amount, noun: N) -> Self {
        Quantity { amount, noun }
    }
    /// An amount the other constructors can't write, as in "6½ losers".
    /// The noun agrees with `number`.
    pub fn other(amount: impl Display, number: Number, noun: N) -> Self {
        Quantity::new(Amount::Other(amount.to_string(), number), noun)
    }
    /// Whether this is one or many, for a verb to agree with.
    pub fn number(&self) -> Number {
        match self.amount {
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

impl<M, N: Countable<M>> AddTo<M> for Quantity<N> {
    fn add_to(self, text: &mut Text<M>) {
        let number = self.number();
        text.push_words(&format!("{} ", self.amount));
        self.noun.add_agreeing(number, text);
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
