# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Workspace layout

A Cargo workspace with four bridge crates, layered bottom to top, and one general-purpose crate:

- **`bridge-deck`** (repo root, `src/`): the `Card` (8 bits) and `Cards` (64-bit set) types plus hand-evaluation helpers (`HandValuation`, `PerSuit`, `CardMap`). `#![deny(missing_docs)]` is on, so every public item needs a doc comment. HTML/UTF8 rendering lives behind the optional `display-as` feature (`src/display.rs`).
- **`bridge-solver`**: a card-play solver (`Naive`, `Starting`, `Score`). It samples possible deals for the cards nobody can see.
- **`robot`**: the game model (`GameState`, `Seat`, `Seated<T>`, `Bid`, `Action`) and the AI (`src/ai.rs`). It builds as an rlib and also as a `cdylib` for WebAssembly.
- **`abridge`**: the axum web server for the online bridge game (abridgegame.com). `abridge/src/lib.rs` holds the tables, templates and websocket logic, and `abridge/src/axum_server.rs` holds the routes and TLS, whose certificate `rustls-acme` obtains from Let's Encrypt.
- **`wording`**: builds English sentences from pieces that know their own grammar into a `Text`, the one type of phrase, which displays as HTML, UTF8 or LaTeX through display-as. Quantities (`count`, `at_least`, `between`, or `5 * noun`) make their head noun agree, `agree` makes a predicate's verbs (`Word::verb`) agree with its subject, `a` picks "a" or "an" when displayed, and `List` joins items. It knows nothing about bridge, so that it can move to its own repository later; bridge's bids and suits implement `wording::Markup`, which has one method per format.

## Commands

```bash
cargo test -p bridge-deck -p bridge-solver -p robot -p wording   # all real tests (avoid --workspace, see below)
cargo test -p robot test_sheets                        # single test by name filter
cargo run -p bridge-solver --release --bin time        # quick solver timing
./run.sh                                               # build robot wasm, then run the server on :8087
```

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

**Bidding conventions are regexes over the auction.** `bids_string` turns the auction into tokens such as `P 1C X 2N XX`. Each `Convention` variant matches that string with a `RegexSet` and holds min/max `HandValuation` bounds plus a `Forcing` level. `Convention::sheets()` builds the single convention set in use. It is long and generated programmatically. `ConventionalBid` tries each forcing level from strongest to weakest and, within a level, legal bids from highest to lowest. It picks the first bid whose bounds fit the hand. The same conventions power the bid descriptions and advice shown to humans (`bid_convention`, `bid_understood2`).

**Card play** uses `bridge_solver::Naive`, with more sampling as fewer tricks remain. See `RandomPlay` in `ai.rs`. Opening leads use the heuristic `opening_lead` instead.

## Conventions worth knowing

- `Card` encoding is `offset = rank + 16 * suit`, with rank 2–14 (ace = 14) and suits ordered Clubs < Diamonds < Hearts < Spades. `Cards` is a `u64` bitset over those offsets, so set operations (`+`, `-`, `&`, `in_suit`) are single bit operations.
- `Seat` is `South=0, West=1, North=2, East=3`. `Seat + n` wraps modulo 4 and `next()` moves clockwise. Index per-seat data with `Seated<T>`, not raw arrays.
- `bridge_solver::Starting.hands` are ordered relative to the **leader of the current trick**, not by `Seat`. `GameState::starting()` does the rotation.
- HTML templates use `display-as` with `[% ... %]` delimiters, for example `#[with_template("[%" "%]" "player.html")]`. The template files sit next to the Rust source that names them, and the code inside the delimiters is Rust. Templates and `style.css` are compiled into the binary, so you must rebuild to see changes.
- `clean_tables` runs every 10 minutes. It closes a table once nothing has happened there for an hour, or for 10 minutes when nobody is sitting at it, and tells any tabs still connected (close code 4000) not to reconnect. Anything a player or the robot does, and every connect or disconnect, counts as activity (`GameState::touch`).
