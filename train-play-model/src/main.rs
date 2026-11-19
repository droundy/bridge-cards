//! Trains the play model further by self-play and publishes the result.
//!
//! It takes the play model file that `robot/play-model.txt` names (fetching it
//! if need be; see `fetch-play-model.sh`), plays more rounds of self-play from
//! it (see `self_play` in `robot/src/ai/fitted_play.rs`), as a population of
//! learners in a round robin of mixed partnerships on fresh deals, and then
//! plays the new best model against the old one in a two-table match of 2000
//! deals.  If the new model is no worse there, or with `--force`, it uploads
//! the new play model file to the GitHub release `play-models` and points
//! `robot/play-model.txt` at it, for you to commit.  Uploading publishes the
//! file: the repository is public.
//!
//! ```text
//! cargo run --release -p train-play-model -- [--rounds 500] [--threads N]
//!     [--learners 4] [--shapes v2,...] [--from-scratch] [--force]
//! ```
//!
//! The learners that the file lacks take the shapes in turn (see `Shape` in
//! `robot/src/ai/fitted_play.rs`), and with `--from-scratch` every learner
//! starts from nothing, so that shapes can be compared on the same training.
//! At each snapshot every learner is also scored against the robot and the old
//! model, which do not learn.  The files of a run are in
//! `target/train-play-model`.

use sha2::{Digest, Sha256};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// What a run is asked to do.
struct Options {
    rounds: usize,
    threads: usize,
    learners: usize,
    shapes: String,
    from_scratch: bool,
    force: bool,
}

impl Options {
    fn from_args() -> Result<Options, String> {
        let mut options = Options {
            rounds: 500,
            threads: std::thread::available_parallelism().map_or(4, |n| n.get()),
            learners: 4,
            shapes: "v2".into(),
            from_scratch: false,
            force: false,
        };
        let mut args = std::env::args().skip(1);
        while let Some(arg) = args.next() {
            let mut value = || args.next().ok_or(format!("{arg} needs a value"));
            let number = |value: String| value.parse().map_err(|_| format!("{arg} takes a number, not {value}"));
            match arg.as_str() {
                "--rounds" => options.rounds = number(value()?)?,
                "--threads" => options.threads = number(value()?)?,
                "--learners" => options.learners = number(value()?)?,
                "--shapes" => options.shapes = value()?,
                "--from-scratch" => options.from_scratch = true,
                "--force" => options.force = true,
                _ => return Err(format!("unknown argument {arg}")),
            }
        }
        Ok(options)
    }
}

fn main() {
    let options = Options::from_args().unwrap_or_else(|e| fail(&e));
    // The top of the repository, where the scripts and cargo are run.
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_path_buf();
    std::env::set_current_dir(&root).unwrap();
    let dir = root.join("target/train-play-model");
    std::fs::create_dir_all(&dir).unwrap();
    let installed = root.join("robot/src/ai/play_model.bin");

    run(Command::new("./fetch-play-model.sh").arg("--exact"));
    let old = dir.join("old.bin");
    std::fs::copy(&installed, &old).unwrap();
    // A file left from an earlier run must not be taken for this one's.
    let new = dir.join("new.bin");
    let _ = std::fs::remove_file(&new);

    println!("Training for {} rounds on {} threads.", options.rounds, options.threads);
    let mut training = robot_test("self_play");
    if !options.from_scratch {
        training.env("PLAY_SELF_RESUME", &old);
    }
    training
        .env("PLAY_SELF_CHECKPOINT", &new)
        .env("PLAY_SELF_REFERENCE", &old)
        .env("PLAY_SELF_WATCH", dir.join("watch.html"))
        .env("PLAY_SELF_ROUNDS", options.rounds.to_string())
        .env("PLAY_THREADS", options.threads.to_string())
        .env("PLAY_SELF_LEARNERS", options.learners.to_string())
        .env("PLAY_SELF_SHAPES", &options.shapes);
    for (name, value) in [
        ("PLAY_SELF_REPORT", "20"),
        ("PLAY_SELF_ROUND_ROBIN", "1"),
        ("PLAY_SELF_WILDCARDS", "4"),
        ("PLAY_SELF_DEALS", "128"),
        ("PLAY_SELF_POOL", "128"),
        ("PLAY_SELF_FRESH", "128"),
        ("PLAY_SELF_ENTROPY", "0.25"),
        ("PLAY_SELF_DECAY", "0.01"),
        ("PLAY_SELF_PAST", "0.2"),
        ("PLAY_SELF_RANDOM", "0.05"),
        ("PLAY_SELF_SNAPSHOT", "100"),
        ("PLAY_SELF_TOURNAMENT", "200"),
    ] {
        training.env(name, value);
    }
    run_showing(&mut training, |line| {
        ["round", "resuming", "learners"].iter().any(|start| line.starts_with(start)) || line.contains("panicked")
    });
    if std::fs::metadata(&new).map_or(true, |m| m.len() == 0) {
        fail("training wrote no model file");
    }

    println!("Playing the new model against the old one.");
    let mut matched = robot_test("match_against_the_robot");
    matched
        .env("PLAY_MATCH_WEIGHTS", &new)
        .env("PLAY_MATCH_AGAINST", &old)
        .env("PLAY_MATCH_DEALS", "2000")
        .env("PLAY_THREADS", options.threads.to_string());
    let result = run_showing(&mut matched, |line| line.contains("deals in"));
    let gain: f64 = result
        .iter()
        .find_map(|line| line.split("took ").nth(1)?.split(' ').next()?.parse().ok())
        .unwrap_or_else(|| fail("could not read the gain from the match"));
    if gain < 0.0 && !options.force {
        fail("the new model is worse than the old one, so it is not published (--force publishes it anyway)");
    }

    let bytes = std::fs::read(&new).unwrap();
    // The round is the first number after the 8 bytes of the magic (see
    // robot/src/ai/fitted_play/model_file.rs).
    let round = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
    let name = format!("play_model-round-{round:06}.bin");
    let published: PathBuf = dir.join(&name);
    std::fs::copy(&new, &published).unwrap();
    run(Command::new("gh").args(["release", "upload", "play-models"]).arg(&published));
    let sum: String = Sha256::digest(&bytes).iter().map(|b| format!("{b:02x}")).collect();
    std::fs::write(
        root.join("robot/play-model.txt"),
        format!(
            "# The play model file that the robot's build includes, kept as an asset of the\n\
             # GitHub release play-models rather than in git: fetch-play-model.sh fetches it\n\
             # and checks it against this SHA-256, and train-play-model replaces it.\n\
             url https://github.com/droundy/bridge-cards/releases/download/play-models/{name}\n\
             sha256 {sum}\n"
        ),
    )
    .unwrap();
    std::fs::copy(&published, &installed).unwrap();
    println!("Published {name}; commit robot/play-model.txt to use it.");
}

/// The ignored robot test `name`, built for release, to be run.
fn robot_test(name: &str) -> Command {
    let mut command = Command::new(std::env::var("CARGO").unwrap_or("cargo".into()));
    command
        .args(["test", "-p", "robot", "--release", name, "--", "--ignored", "--exact", "--nocapture"])
        .arg(format!("ai::fitted_play::tests::{name}"));
    command
}

/// Runs `command`, and stops here if it fails.
fn run(command: &mut Command) {
    let status = command.status().unwrap_or_else(|e| fail(&format!("could not run {command:?}: {e}")));
    if !status.success() {
        fail(&format!("{command:?} failed"));
    }
}

/// Runs `command`, printing the lines of its output that `shown` picks as
/// they come, and gives them; stops here if it fails.
fn run_showing(command: &mut Command, shown: impl Fn(&str) -> bool) -> Vec<String> {
    let mut child = command
        .stdout(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| fail(&format!("could not run {command:?}: {e}")));
    let mut lines = Vec::new();
    for line in BufReader::new(child.stdout.take().unwrap()).lines() {
        let line = line.unwrap();
        if shown(&line) {
            println!("{line}");
            lines.push(line);
        }
    }
    if !child.wait().unwrap().success() {
        fail(&format!("{command:?} failed"));
    }
    lines
}

/// Stops with `message`.
fn fail(message: &str) -> ! {
    eprintln!("train-play-model: {message}");
    std::process::exit(1)
}
