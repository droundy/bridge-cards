pub fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    //
    // Off wasm the hook would only print the panic to stderr, replacing the
    // standard hook (which also names the thread and honors RUST_BACKTRACE),
    // so we install it only on wasm.
    #[cfg(all(feature = "console_error_panic_hook", target_arch = "wasm32"))]
    console_error_panic_hook::set_once();
}

/// HTML as plain text: without its tags, and with its escapes undone.
pub(crate) fn plain(html: &str) -> String {
    let mut text = String::new();
    let mut in_tag = false;
    let mut chars = html.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            // A tag starts with a letter or a slash, so a lone "<" is text,
            // and so is a ">" that does not end a tag, as in "♥>♠".
            '<' if chars
                .peek()
                .map_or(false, |&n| n.is_alphabetic() || n == '/') =>
            {
                in_tag = true
            }
            '>' if in_tag => in_tag = false,
            c if !in_tag => text.push(c),
            _ => {}
        }
    }
    [
        ("&lt;", "<"),
        ("&gt;", ">"),
        ("&quot;", "\""),
        ("&#x27;", "'"),
        ("&#x2f;", "/"),
        ("&amp;", "&"),
    ]
    .iter()
    .fold(text, |text, (escaped, c)| text.replace(escaped, c))
}

#[test]
fn plain_keeps_the_text_of_html() {
    assert_eq!(
        plain("<b>2&#x2f;1</b> ♥>♠ &amp; <span class=\"x\">a</span> 3 < 4"),
        "2/1 ♥>♠ & a 3 < 4"
    );
}
