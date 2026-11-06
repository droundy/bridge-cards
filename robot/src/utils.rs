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
