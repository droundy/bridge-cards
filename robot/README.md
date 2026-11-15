# robot

The game model and bridge AI for the bridge workspace.

- `GameState`, `Seat`, `Seated<T>`, `Bid` and `Action` describe a table and
  its play.
- `src/ai.rs` holds the bidding conventions and the card-play AI.

The crate builds as an rlib, for `abridge` and the tests, and as a `cdylib`
for WebAssembly, which the browser tab used for robot seats runs.

To build the WebAssembly package that `abridge` embeds, run from this
directory:

```bash
wasm-pack build --target web --no-typescript --release
```

`run.sh` at the repository root does this and then starts the server. The
`pkg/` directory it writes is gitignored.

Test the crate from the repository root with:

```bash
cargo test -p robot
```
