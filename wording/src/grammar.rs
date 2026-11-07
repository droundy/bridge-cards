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

/// An amount of a noun, as in "1 spade", "7 spades" or "6–9 points".
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Quantity<A> {
    amount: A,
    number: Number,
    noun: Noun,
}

/// `n` of `noun`, as in "1 spade" or "7 spades".
pub fn count(n: u64, noun: Noun) -> Quantity<u64> {
    Quantity {
        amount: n,
        number: Number::of(n),
        noun,
    }
}

/// `amount` of `noun`, when the amount is not a plain count, as in "6–9
/// points" or "at most 1 spade".  The noun agrees with `number`.
pub fn quantity<A: Display>(amount: A, number: Number, noun: Noun) -> Quantity<A> {
    Quantity {
        amount,
        number,
        noun,
    }
}

impl<A> Quantity<A> {
    /// Whether this is one or many, for a verb to agree with.
    pub fn number(&self) -> Number {
        self.number
    }
}

impl<A: Display> Display for Quantity<A> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {}", self.amount, self.noun.agreeing(self.number))
    }
}

impl<M, A: Display> AddTo<M> for Quantity<A> {
    fn add_to(self, text: &mut Text<M>) {
        text.push_words(&self.to_string());
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
