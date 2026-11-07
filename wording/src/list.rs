use crate::{AddTo, Text};

/// Items joined into a list, as in "a, b and c".
///
/// ```
/// use display_as::{format_as, UTF8};
/// use wording::{text, List, Text};
///
/// let t: Text = text!(List::or(vec!["clubs", "diamonds", "hearts"]));
/// assert_eq!(format_as!(UTF8, t).into_string(), "clubs, diamonds or hearts");
/// ```
pub struct List<T> {
    items: Vec<T>,
    separator: &'static str,
    last: &'static str,
}

impl<T> List<T> {
    /// "a", "a and b", "a, b and c".
    pub fn and(items: Vec<T>) -> Self {
        List::with(items, ", ", " and ")
    }
    /// "a", "a or b", "a, b or c".
    pub fn or(items: Vec<T>) -> Self {
        List::with(items, ", ", " or ")
    }
    /// "a", "a, then b", "a, then b, then c".
    pub fn then(items: Vec<T>) -> Self {
        List::with(items, ", then ", ", then ")
    }
    /// Items joined by `separator`, except that `last` comes before the last.
    pub fn with(items: Vec<T>, separator: &'static str, last: &'static str) -> Self {
        List {
            items,
            separator,
            last,
        }
    }
    /// Is the list empty?
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    /// How many items are in the list?
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl<M, T: AddTo<M>> AddTo<M> for List<T> {
    fn add_to(self, text: &mut Text<M>) {
        let n = self.items.len();
        for (i, item) in self.items.into_iter().enumerate() {
            if i + 1 == n && i > 0 {
                text.push_words(self.last);
            } else if i > 0 {
                text.push_words(self.separator);
            }
            item.add_to(text);
        }
    }
}
