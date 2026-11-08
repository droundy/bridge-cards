use crate::{text, AddTo, Text};
use std::fmt::{self, Display, Formatter};

/// Whether something is one or many, which nouns and verbs agree with.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

/// A noun such as "point" and "points", or a verb such as "is" and "are".
///
/// A noun agrees with the amount of a quantity it is the head of, and a verb
/// with the noun phrase it is in or the subject given to [`agree`].  Either
/// is singular until then.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Word {
    singular: &'static str,
    plural: &'static str,
    noun: bool,
}

/// "is" or "are".
pub const IS: Word = Word::verb("is", "are");
/// "has" or "have".
pub const HAS: Word = Word::verb("has", "have");

impl Word {
    /// A noun such as `Word::noun("spade", "spades")`.
    pub const fn noun(singular: &'static str, plural: &'static str) -> Self {
        Word {
            singular,
            plural,
            noun: true,
        }
    }
    /// A verb such as `Word::verb("shows", "show")`, whose singular is the
    /// form that goes with "it".
    pub const fn verb(singular: &'static str, plural: &'static str) -> Self {
        Word {
            singular,
            plural,
            noun: false,
        }
    }
    pub(crate) fn form(self, number: Number) -> &'static str {
        match number {
            Number::Singular => self.singular,
            Number::Plural => self.plural,
        }
    }
    pub(crate) fn is_noun(self) -> bool {
        self.noun
    }
}

impl AddTo for Word {
    fn add_to(self, text: &mut Text) {
        text.push_word(self);
    }
}

/// `predicate` agreeing with `subject`, as in "partner has 5+ spades" or
/// "North and South have bid".
///
/// The verbs in `predicate` agree with the subject's
/// [number](Text::number), and nothing else does: in "partner is short in
/// hearts" the hearts stay plural.  A clause already made with `agree` keeps
/// its own verbs, as in "you know that partner has 5+ spades", and so does a
/// noun phrase, whose verbs agree with it, as in "you have a hand that is
/// balanced".
pub fn agree(subject: impl AddTo, predicate: impl AddTo) -> Text {
    let subject = text!(subject);
    let mut predicate = text!(predicate);
    predicate.settle_verbs(subject.number());
    text!(subject, " ", predicate)
}

/// `phrase`, which is one or many as `number` says, as in
/// `numbered("partner", Number::Singular)` or `numbered("you",
/// Number::Plural)`.  Its first noun and its verbs agree with `number`, and
/// so do the verbs [`agree`] gives it.
pub fn numbered(phrase: impl AddTo, number: Number) -> Text {
    let mut text = text!(phrase);
    text.settle(number);
    text
}

/// How many there are of something: a whole number such as `Amount::from(5)`,
/// or a number of halves such as `Amount::halves(13)` for "6½".
///
/// A negative `i32` panics.  `i32` is there so that a plain `5` can be an
/// amount.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Amount {
    halves: u64,
}

impl Amount {
    /// `n` halves, as in `Amount::halves(13)` for "6½".
    pub const fn halves(n: u64) -> Self {
        Amount { halves: n }
    }
    fn is_zero(self) -> bool {
        self.halves == 0
    }
    /// Singular for exactly one, plural otherwise: "1 loser", "½ losers",
    /// "1½ losers".
    fn number(self) -> Number {
        if self.halves == 2 {
            Number::Singular
        } else {
            Number::Plural
        }
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match (self.halves / 2, self.halves % 2) {
            (0, 1) => f.write_str("½"),
            (whole, 1) => write!(f, "{whole}½"),
            (whole, _) => write!(f, "{whole}"),
        }
    }
}

macro_rules! amount_from {
    ($($int:ty),*) => {
        $(impl From<$int> for Amount {
            fn from(n: $int) -> Self {
                let n = u64::try_from(n).expect("an amount is never negative");
                Amount::halves(n.checked_mul(2).expect("an amount fits in 63 bits"))
            }
        })*
    };
}
amount_from!(u8, u16, u32, u64, usize, i32);

/// `amount`, then `noun`, as a noun phrase that is one or many as `number`
/// says.
fn quantity(amount: impl Display, number: Number, noun: impl AddTo) -> Text {
    let mut text = Text::new();
    text.push_words(&format!("{amount} "));
    noun.add_to(&mut text);
    text.settle(number);
    text
}

/// `n` of `noun`, as in "1 spade", "7 spades" or "no spades".
///
/// The noun is any phrase.  Its first noun agrees with the amount, as in "7
/// high card points in spades", and so do its verbs, as in "5 cards that
/// are high".  A clause in it with a subject of its own needs [`agree`].
/// Multiplying does the same: `n * noun` is `count(n, noun)`, and so on for
/// the other quantities:
///
/// ```
/// use wording::{text, Word};
///
/// const POINT: Word = Word::noun("point", "points");
/// let n: u8 = 3;
/// let t = text!(1 * POINT, ", ", n * POINT, ", ", 0 * POINT, ", ")
///     + (5..) * POINT
///     + ", "
///     + (..=1) * POINT
///     + ", "
///     + (4..=4) * POINT
///     + " or "
///     + (6..=9) * text!("high card ", POINT);
/// assert_eq!(
///     t.to_string(),
///     "1 point, 3 points, no points, 5+ points, at most 1 point, \
///      exactly 4 points or 6–9 high card points"
/// );
/// ```
///
/// [`Word`] and [`Text`] multiply already, and
/// [`countable!`](crate::countable!) makes your own markup multiply.
pub fn count(n: impl Into<Amount>, noun: impl AddTo) -> Text {
    let n = n.into();
    if n.is_zero() {
        quantity("no", Number::Plural, noun)
    } else {
        quantity(n, n.number(), noun)
    }
}

/// `n` or more of `noun`, as in "5+ spades".
pub fn at_least(n: impl Into<Amount>, noun: impl AddTo) -> Text {
    quantity(format_args!("{}+", n.into()), Number::Plural, noun)
}

/// `n` or fewer of `noun`, as in "at most 1 spade", or "no spades" when `n`
/// is zero.
pub fn at_most(n: impl Into<Amount>, noun: impl AddTo) -> Text {
    let n = n.into();
    if n.is_zero() {
        count(n, noun)
    } else {
        quantity(format_args!("at most {n}"), n.number(), noun)
    }
}

/// From `lo` to `hi` of `noun`, as in "6–9 points".  When `lo` is zero it is
/// [`at_most`], and when the two are equal it is "exactly 4 points" (or "no
/// points").
pub fn between(lo: impl Into<Amount>, hi: impl Into<Amount>, noun: impl AddTo) -> Text {
    let (lo, hi) = (lo.into(), hi.into());
    if lo == hi && !lo.is_zero() {
        quantity(format_args!("exactly {lo}"), lo.number(), noun)
    } else if lo.is_zero() {
        at_most(hi, noun)
    } else {
        quantity(format_args!("{lo}–{hi}"), Number::Plural, noun)
    }
}

/// Lets a number or a range multiply a noun into a quantity: `5 * noun` is
/// [`count`], `(5..) * noun` [`at_least`], `(..=1) * noun` [`at_most`] and
/// `(6..=9) * noun` [`between`].
///
/// [`Word`] and [`Text`] already multiply.  Use this for markup of your own,
/// as in `wording::countable!(Suit)`.  Rust allows these impls only in the
/// crate that defines the noun's type.  A negative number panics.
///
/// A range of any integer type has one impl, so its product is known to be a
/// [`Text`] at once:
///
/// ```
/// use wording::Word;
///
/// const POINT: Word = Word::noun("point", "points");
/// let points = (5..) * POINT;
/// assert_eq!(points.to_string(), "5+ points");
/// ```
///
/// A bare number has an impl for each integer type instead, because Rust
/// allows no generic one, so `let t = 5 * POINT;` needs `let t: Text` when
/// `t` is used before the number's type is known.
#[macro_export]
macro_rules! countable {
    ($noun:ty) => {
        $crate::countable!(@each $noun; u8, u16, u32, u64, usize, i32);
        impl<T: ::core::convert::Into<$crate::Amount>> ::core::ops::Mul<$noun>
            for ::core::ops::RangeFrom<T>
        {
            type Output = $crate::Text;
            fn mul(self, noun: $noun) -> $crate::Text {
                $crate::at_least(self.start, noun)
            }
        }
        impl<T: ::core::convert::Into<$crate::Amount>> ::core::ops::Mul<$noun>
            for ::core::ops::RangeToInclusive<T>
        {
            type Output = $crate::Text;
            fn mul(self, noun: $noun) -> $crate::Text {
                $crate::at_most(self.end, noun)
            }
        }
        impl<T: ::core::convert::Into<$crate::Amount>> ::core::ops::Mul<$noun>
            for ::core::ops::RangeInclusive<T>
        {
            type Output = $crate::Text;
            fn mul(self, noun: $noun) -> $crate::Text {
                let (lo, hi) = self.into_inner();
                $crate::between(lo, hi, noun)
            }
        }
    };
    (@each $noun:ty; $($int:ty),*) => {
        $( $crate::countable!(@one $noun; $int); )*
    };
    (@one $noun:ty; $int:ty) => {
        impl ::core::ops::Mul<$noun> for $int {
            type Output = $crate::Text;
            fn mul(self, noun: $noun) -> $crate::Text {
                $crate::count(self, noun)
            }
        }
    };
}

countable!(Word);
countable!(Text);

/// `phrase` with "a" or "an" in front, as in "an ace", "a king" or "an
/// 8-card fit".  The phrase is singular, its first noun and its verbs too,
/// as in "a hand with 5+ spades is strong".
///
/// The choice is made when the text is displayed, from whatever is shown
/// first, so it works before any phrase, and [`Markup`](crate::Markup)
/// decides for itself.  It goes by spelling, with exceptions for the usual
/// words that start with a silent "h" ("an honor") or with a "y" or "w" sound
/// ("a one", "a unit", "a European"), for numbers ("an 8", "an 11", "an
/// 18"), and for initials ("an NT bid", "an HCP").  Other exceptions need the
/// article written out.
pub fn a(phrase: impl AddTo) -> Text {
    let mut text = Text::new();
    text.push_article();
    phrase.add_to(&mut text);
    text.settle(Number::Singular);
    text
}

/// Does `words` start with a vowel sound, so that "an" goes before it?
pub(crate) fn vowel_sound(words: &str) -> bool {
    let first = words
        .split(|c: char| !c.is_alphanumeric())
        .next()
        .unwrap_or("");
    if first.starts_with(|c: char| c.is_ascii_digit()) {
        // "eight", "eleven" and "eighteen", and "eighty" and "eight hundred".
        let digits: String = first.chars().take_while(char::is_ascii_digit).collect();
        return digits.starts_with('8') || digits == "11" || digits == "18";
    }
    let vowels = |c: char| "AEIOUaeiou".contains(c);
    if first.chars().all(char::is_uppercase) && !first.contains(vowels) {
        // Initials read as letters, such as "NT" or "HCP".
        return first.starts_with(|c: char| "AEFHILMNORSX".contains(c));
    }
    let lower = first.to_lowercase();
    const SILENT_H: [&str; 5] = ["hour", "honest", "honor", "honour", "heir"];
    const CONSONANT_SOUND: [&str; 12] = [
        "once", "uniq", "unit", "unif", "unio", "univ", "unic", "unis", "use", "usu", "uti", "eu",
    ];
    if SILENT_H.iter().any(|p| lower.starts_with(p)) {
        true
    } else if lower == "one" || CONSONANT_SOUND.iter().any(|p| lower.starts_with(p)) {
        false
    } else {
        lower.starts_with(vowels)
    }
}
