
use crate::{Card, Cards};

use display_as::{with_template, format_as, DisplayAs, HTML, UTF8};

#[with_template("[%" "%]" "card.html")]
impl DisplayAs<HTML> for Card {}

impl DisplayAs<UTF8> for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.unicode())
    }
}

#[with_template("[%" "%]" "cards.html")]
impl DisplayAs<HTML> for Cards {}

#[with_template("[%" "%]" "cards.txt")]
impl DisplayAs<UTF8> for Cards {}

#[test]
fn disp() {
    assert_eq!("🃑", &format_as!(UTF8, Card::CA));
    assert_eq!(r#"<span class="clubs">🃑</span>"#, &format_as!(HTML, Card::CA));

    // println!("{}", format_as!(UTF8, Cards::ALL));
    // assert_eq!(r#"<span class="clubs">🃑</span>"#, &format_as!(UTF8, Cards::ALL));
    // assert_eq!(r#"<span class="clubs">🃑</span>"#, &format_as!(HTML, Cards::ALL));
}
