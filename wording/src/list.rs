use crate::{Number, Text};

/// `items` as a list: "a", "a and b" or "a, b and c".  Two or more are
/// plural as a subject.
///
/// ```
/// use wording::{agree, and, or, text, Word};
///
/// let suits = or(["clubs", "diamonds", "hearts"]);
/// assert_eq!(suits.to_string(), "clubs, diamonds or hearts");
///
/// let bidders = and(["North", "South"]);
/// assert_eq!(agree(bidders, text!(Word::IS, " silent")).to_string(), "North and South are silent");
/// ```
pub fn and(items: impl IntoIterator<Item = impl Into<Text>>) -> Text {
    join(items, ", ", " and ", Some(Number::Plural))
}

/// `items` as a list of choices: "a", "a or b" or "a, b or c".  As a
/// subject, a verb agrees with the last, as in "partner or you are".
pub fn or(items: impl IntoIterator<Item = impl Into<Text>>) -> Text {
    join(items, ", ", " or ", None)
}

/// `items` one after another: "a", "a, then b" or "a, then b, then c".
pub fn then(items: impl IntoIterator<Item = impl Into<Text>>) -> Text {
    join(items, ", then ", ", then ", None)
}

/// `items` joined by `separator`, except that `last` comes before the last.
/// As a subject, two or more are `many`, if given, and otherwise the list
/// agrees with its last item.
fn join(
    items: impl IntoIterator<Item = impl Into<Text>>,
    separator: &str,
    last: &str,
    many: Option<Number>,
) -> Text {
    let items: Vec<Text> = items.into_iter().map(Into::into).collect();
    let number = match (many, items.last()) {
        (Some(many), _) if items.len() > 1 => many,
        (_, Some(item)) => item.number(),
        (_, None) => Number::Plural,
    };
    let n = items.len();
    let mut list = Text::new();
    for (i, item) in items.into_iter().enumerate() {
        if i > 0 {
            list.push_words(if i + 1 == n { last } else { separator });
        }
        list += item;
    }
    list.mark_number(number);
    list
}
