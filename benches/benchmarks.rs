//! Runs `cargo bench` in `benchmarks/`, passing on any arguments, so that
//! `cargo bench -- "full deal"` works from the top of the repository.
//!
//! The benchmarks are their own workspace, and they have to be run from inside
//! that directory for its `.cargo/config.toml` (function alignment) to apply.

use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    let args: Vec<_> = std::env::args_os().skip(1).collect();
    // `cargo bench` passes `--bench`, and `cargo test --benches` does not:
    // a test run should not take five minutes.
    if !args.iter().any(|a| a == "--bench") {
        return ExitCode::SUCCESS;
    }
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let status = Command::new(cargo)
        .args(["bench", "--"])
        .args(args.iter().filter(|a| *a != "--bench"))
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/benchmarks"))
        .status();
    match status {
        Ok(status) if status.success() => ExitCode::SUCCESS,
        Ok(_) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("could not run cargo bench in benchmarks/: {e}");
            ExitCode::FAILURE
        }
    }
}
