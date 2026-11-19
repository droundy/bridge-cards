//! Puts the weights of the play model where the build includes them, from
//! the play model file `src/ai/play_model.bin`, which is not kept in git (see
//! `fetch-play-model.sh` and `src/ai/fitted_play/model_file.rs`).  The
//! weights come first in the file, after a header: the 8 bytes `PLAYMDL1`
//! and seven little-endian `u32`s, the first of them the number of weights.
//! With no file, nothing is included, and the model's player plays as the
//! robot does.

fn main() {
    let source = std::path::Path::new("src/ai/play_model.bin");
    println!("cargo:rerun-if-changed={}", source.display());
    let out = std::path::Path::new(&std::env::var("OUT_DIR").unwrap()).join("play_model.bin");
    let bytes = std::fs::read(source).unwrap_or_default();
    let weights = if bytes.starts_with(b"PLAYMDL1") {
        let n = u32::from_le_bytes(bytes[8..12].try_into().unwrap()) as usize;
        bytes[36..36 + 4 * n].to_vec()
    } else {
        Vec::new()
    };
    std::fs::write(out, weights).unwrap();
}
