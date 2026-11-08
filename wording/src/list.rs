use crate::{text, AddTo, Number, Text};

/// Items joined into a list, as in "a, b and c".
///
/// ```
/// use wording::{agree, text, List, IS};
///
/// let suits = List::or(["clubs", "diamonds", "hearts"]);
/// assert_eq!(text!(suits).to_string(), "clubs, diamonds or hearts");
///
/// let mut bidders = List::and(["North"]);
/// bidders.push("South");
/// assert_eq!(agree(bidders, text!(IS, " silent")).to_string(), "North and South are silent");
/// ```
#[derive(Clone, Debug)]
pub struct List {
    items: Vec<Text>,
    separator: &'static str,
    last: &'static str,
    /// The number of two or more items as a subject, if it is not the last
    /// item's.
    many: Option<Number>,
}

impl List {
    /// "a", "a and b", "a, b and c".  Two or more are plural as a subject.
    pub fn and(items: impl IntoIterator<Item = impl AddTo>) -> Self {
        List {
            many: Some(Number::Plural),
            ..List::with(items, ", ", " and ")
        }
    }
    /// "a", "a or b", "a, b or c".  As a subject, a verb agrees with the
    /// last, as in "partner or you are".
    pub fn or(items: impl IntoIterator<Item = impl AddTo>) -> Self {
        List::with(items, ", ", " or ")
    }
    /// "a", "a, then b", "a, then b, then c".
    pub fn then(items: impl IntoIterator<Item = impl AddTo>) -> Self {
        List::with(items, ", then ", ", then ")
    }
    /// Items joined by `separator`, except that `last` comes before the last.
    /// As a subject, a verb agrees with the last item.
    pub fn with(
        items: impl IntoIterator<Item = impl AddTo>,
        separator: &'static str,
        last: &'static str,
    ) -> Self {
        List {
            items: items.into_iter().map(|item| text!(item)).collect(),
            separator,
            last,
            many: None,
        }
    }
    /// Adds an item to the end.
    pub fn push(&mut self, item: impl AddTo) {
        self.items.push(text!(item));
    }
    /// Is the list empty?
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    /// How many items are in the list?
    pub fn len(&self) -> usize {
        self.items.len()
    }
    /// Whether the list is one or many as a subject.
    fn number(&self) -> Number {
        match (self.many, self.items.last()) {
            (Some(many), _) if self.items.len() > 1 => many,
            (_, Some(last)) => last.number(),
            (_, None) => Number::Plural,
        }
    }
}

impl AddTo for List {
    fn add_to(self, text: &mut Text) {
        let number = self.number();
        let n = self.items.len();
        let mut list = Text::new();
        for (i, item) in self.items.into_iter().enumerate() {
            if i + 1 == n && i > 0 {
                list.push_words(self.last);
            } else if i > 0 {
                list.push_words(self.separator);
            }
            item.add_to(&mut list);
        }
        list.set_number(number);
        list.add_to(text);
    }
}
