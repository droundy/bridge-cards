# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Workspace layout

A Cargo workspace with four bridge crates, layered bottom to top, and one general-purpose crate:

- **`bridge-deck`** (repo root, `src/`): the `Card` (8 bits) and `Cards` (64-bit set) types plus hand-evaluation helpers (`HandValuation`, `PerSuit`, `CardMap`). `#![deny(missing_docs)]` is on, so every public item needs a doc comment. HTML/UTF8 rendering lives behind the optional `display-as` feature (`src/display.rs`).
- **`bridge-solver`**: a card-play solver (`Naive`, `Starting`, `Score`). It samples possible deals for the cards nobody can see.
- **`robot`**: the game model (`GameState`, `Seat`, `Seated<T>`, `Bid`, `Action`) and the AI (`src/ai.rs`). It builds as an rlib and also as a `cdylib` for WebAssembly.
- **`abridge`**: the axum web server for the online bridge game (abridgegame.com). `abridge/src/lib.rs` holds the tables, templates and websocket logic, and `abridge/src/axum_server.rs` holds the routes and TLS, whose certificate `rustls-acme` obtains from Let's Encrypt.
- **`wording`**: builds English sentences from pieces that know their own grammar into a `Text`, the one type of phrase, which displays as HTML, UTF8 or LaTeX through display-as. Quantities (`count`, `at_least`, `between`, or `5 * noun`) make their head noun and their verbs agree, so a noun phrase reads the same in any sentence; a verb (`Word::verb`) added with `+` agrees with the subject of the text before it (`partner + Word::HAS + "bid"`; a clause inside a clause goes in parentheses), `+` spaces words that would otherwise run together, `a` picks "a" or "an" when displayed, and `and`, `or` and `then` join items. Every function takes its pieces as `impl Into<Text>` and returns a `Text`; the only other public types are `Word` (a `const` noun or verb), `Amount` (halves, as in "6½"), and `Markup`, `Form` and `Number` for markup. It knows nothing about bridge, so that it can move to its own repository later; a type such as a bid or a suit implements `wording::Markup`, which has one method per format, and calls `wording::markup!` so that it multiplies into quantities and starts a text with `+` (`Suit` does, behind bridge-deck's `wording` feature, and so does `Bid`). A text that starts with a verb is a predicate whose verbs all agree with the subject it is added to, so a relative clause on its object, as in "has a card that is high", goes in `a`, `count` or parentheses. There is no `text!` macro: a text starts from a `Word`, markup, a string followed by a `Word` or `Text`, or `Text::from(...)`.

## Commands

```bash
cargo test -p bridge-deck -p bridge-solver -p robot -p wording   # all real tests (avoid --workspace, see below)
cargo test -p robot test_sheets                        # single test by name filter
cargo run -p bridge-solver --release --bin time        # quick solver timing
./run.sh                                               # build robot wasm, then run the server on :8087
prek run --all-files                                   # quick checks, including clippy with warnings as errors
```

CI (`.github/workflows/warnings.yml`) runs clippy over everything with `RUSTFLAGS=-D warnings`, so a rustc or clippy warning fails the PR. `.pre-commit-config.yaml` runs the same check quickly under [prek](https://github.com/j178/prek) (`prek install` to run it on every commit); it uses `target/prek` so that its flags don't invalidate your normal build.

Benchmarks are written in each crate's `src/benches.rs` and only compiled with the crate's `benchmarks` feature. They are run by `benchmarks/`, a separate crate with its own `[workspace]`, which has to be run from inside that directory so that its `.cargo/config.toml` (function alignment) applies. `cargo bench` at the top of the repository does that for you (`benches/benchmarks.rs` runs `cargo bench` in `benchmarks/` and passes on its arguments):

```bash
cargo bench                   # every benchmark
cargo bench -- "full deal"    # groups whose names contain "full deal"
```

The runner links both the working tree and the committed `main` branch on GitHub (the `bridge-deck-main` and `bridge-solver-main` git dev-dependencies), so scaling registers every benchmark twice and reports each one against its `main` version. When the version numbers match, scaling tells the copies apart by source: `naive@git` is `main`, and the baseline, and `naive@local` is the working tree. `benchmarks/Cargo.lock` pins which commit of `main` is used, and `cargo bench` at the top runs `cargo update` in `benchmarks/` first, so the baseline is always the current `main` (a failed update, say offline, only warns). Running `cargo bench` inside `benchmarks/` itself skips the update. The runner is kept out of the main workspace so that the server and robot still build when GitHub's `main` lacks the `benchmarks` feature, and because the second `bridge-deck` and `bridge-solver` would make `cargo update -p bridge-deck` ambiguous.

Inputs must have the same type in both versions, so they are plain integers. `Cards`, `Suit` and `Starting` convert to and from them with `into_for_bench` and `from_for_bench`. Keep that encoding the same in every version. Scaling keeps only the newest version's inputs, so both versions are handed the same cards. Don't use `#[scaling::bench_scaling]` on the solver. It picks its own sizes with no upper limit, and solver cost is exponential in the number of cards, so it ends up in calls that take hours. List explicit `sizes(..)` instead.

`abridge` embeds `robot/pkg/robot.js` and `robot/pkg/robot_bg.wasm` with `include_bytes!`. That directory is gitignored and only exists after you run `wasm-pack build --target web --no-typescript --release` in `robot/`, which `run.sh` does. Until it exists, any build of `abridge` (including `cargo test --workspace`) fails. After starting the server, open `http://localhost:8087/random`.

`deploy.sh` builds a static musl binary and copies it to the production host over scp. `systemd/` restarts the service whenever that binary changes, so running `deploy.sh` ships to production immediately.

## Architecture

**The server renders HTML; the browser renders nothing itself.** Each table is a `GameState<WsSender>` kept in a `DashMap` (`TableMap` in `abridge/src/lib.rs`). On every change, `send_player_updates` renders the `Player` template for each connected seat and sends the full HTML over that seat's websocket. `player-page.html` replaces `#main` with whatever arrives. Clickable elements come from `impl DisplayAs<HTML> for Action`, which outputs an `onclick="send_message('<json Action>')"` attribute. Clicks send JSON `Action`s back, and `ws_connected` validates and applies them.

**The robot runs in a browser tab, not on the server.** `/<table>/<seat>/robot` serves `robot-page.html`, which loads the wasm build of `robot` and opens `/<table>/<seat>/robot/ws`. That connection becomes `GameState.ai`. When it is the turn of a seat with no human connected, the server sends that tab the `serde_json` of `GameState`. Connections and conventions are `#[serde(skip)]`, so the tab receives every hand. The tab calls `choose_play` (`#[wasm_bindgen]` in `robot/src/ai.rs`) and sends the resulting `Action` back like any player. Seats with no human get `PlayerName::Robot` only while an AI tab is connected.

**Bidding conventions are regexes over the auction.** `bids_string` turns the auction into tokens such as `P 1C X 2N XX`. A convention set is a table (`Convention::Ordered`) of entries, each matching that string with a `RegexSet`. An entry holds min/max `HandValuation` bounds (`Simple`), or several `Fit`s whose bounds can carry `ShapeRule`s such as "spades at least as long as hearts" or balanced (`Rule`), plus a `Forcing` level that only says what the bid means. The table's order is the robot's preference: `ConventionalBid` bids the first entry that matches a legal bid and fits the hand. Every entry that matches a bid describes it, so a later entry for the same bid is a fallback, and partner learns the hull of them all; `Natural` and `Forced` entries are defaults, used only when no other entry matches. `Convention::sheets()` builds the house convention set. It is long and generated programmatically, and was written for the most forcing bid that fits to win and then the highest, so `most_forcing_then_highest` sorts it into that order. It has no fallbacks (no two of its entries describe the same bid, which `sheets_entries_do_not_overlap` checks), so in that order it bids as it did before tables were read in order. `Convention::sayc()` and `Convention::two_over_one()` (`robot/src/ai/sayc.rs`) are separate sets built by one function and written in preference order, following the ACBL SAYC booklet. They have their own notrump structure, and share the strong 2C, preempt and natural-fallback blocks with `sheets()` through the `add_*` helpers in `ai.rs`, each sorted the Sheets way. A table's `GameState.system` (`BiddingSystem`, serialized so the robot tab sees it) picks which set the robots bid by and the bid descriptions explain; players change it with `Action::SetSystem` before the first bid. The ignored test `dump_auctions` writes seeded auctions to diff a change to the conventions against the old ones. The same conventions power the bid descriptions shown to humans (`bid_convention`, `bid_understood2`) and the bidding advice (`GameState::bidding_advice`, `robot/src/ai/advice.rs`), which puts each bound and shape rule into words with `wording` and suggests the bid the robot would make, found with the same `Convention::first_fit` that `ConventionalBid` bids by, so it never disagrees with the robot. Its "Possible contracts" view (`advice/contracts.rs`) weighs what the natural entries say a partnership needs, such as 26 points for 3NT, against partner's shown range, and says what each bid would tell partner. Everything the advice knows about the other hands is in `Shown`, the place for a better estimate later.

**Card play** uses `bridge_solver::Naive`, with more sampling as fewer tricks remain. See `RandomPlay` in `ai.rs`. Opening leads use the heuristic `opening_lead` instead.

## Conventions worth knowing

- `Card` encoding is `offset = rank + 16 * suit`, with rank 2–14 (ace = 14) and suits ordered Clubs < Diamonds < Hearts < Spades. `Cards` is a `u64` bitset over those offsets, so set operations (`+`, `-`, `&`, `in_suit`) are single bit operations.
- `Seat` is `South=0, West=1, North=2, East=3`. `Seat + n` wraps modulo 4 and `next()` moves clockwise. Index per-seat data with `Seated<T>`, not raw arrays.
- `bridge_solver::Starting.hands` are ordered relative to the **leader of the current trick**, not by `Seat`. `GameState::starting()` does the rotation.
- HTML templates use `display-as` with `[% ... %]` delimiters, for example `#[with_template("[%" "%]" "player.html")]`. The template files sit next to the Rust source that names them, and the code inside the delimiters is Rust. Templates and `style.css` are compiled into the binary, so you must rebuild to see changes.
- `clean_tables` runs every 10 minutes. It closes a table once nothing has happened there for an hour, or for 10 minutes when nobody is sitting at it, and tells any tabs still connected (close code 4000) not to reconnect. Anything a player or the robot does, and every connect or disconnect, counts as activity (`GameState::touch`).
