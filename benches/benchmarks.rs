//! Runs `cargo bench` in `benchmarks/`, passing on any arguments, so that
//! `cargo bench -- "full deal"` works from the top of the repository.  First
//! it runs `cargo update` there, so that the baseline is always the current
//! `main` on GitHub rather than whichever commit `Cargo.lock` last pinned.
//!
//! The benchmarks are their own workspace, and they have to be run from inside
//! that directory for its `.cargo/config.toml` (function alignment) to apply.

use std::ffi::OsString;
use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    let args: Vec<_> = std::env::args_os().skip(1).collect();
    // `cargo bench` passes `--bench`, and `cargo test --benches` does not:
    // a test run should not take five minutes.
    if !args.iter().any(|a| a == "--bench") {
        return ExitCode::SUCCESS;
    }
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    // Without a network the old pin may still work, so don't give up here.
    match cargo_in_benchmarks(&cargo).arg("update").status() {
        Ok(status) if status.success() => {}
        Ok(_) => eprintln!("cargo update failed in benchmarks/, using the old Cargo.lock"),
        Err(e) => eprintln!("could not run cargo update in benchmarks/: {e}"),
    }
    let status = cargo_in_benchmarks(&cargo)
        .args(["bench", "--"])
        .args(args.iter().filter(|a| *a != "--bench"))
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

fn cargo_in_benchmarks(cargo: &OsString) -> Command {
    let mut command = Command::new(cargo);
    command.current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/benchmarks"));
    command
}
