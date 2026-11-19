//! Puts the play model where the build includes it, from the play model file
//! `src/ai/play_model.bin`, which is not kept in git (see `fetch-play-model.sh`
//! and `src/ai/fitted_play/model_file.rs`).  The weights come first in the
//! file, after a header: the 8 bytes `PLAYMDL1` and seven little-endian
//! `u32`s, the first four of them the number of weights and the shape of the
//! network (its entries and the sizes of its two hidden layers).  What is
//! included is those four words and then the weights.  With no file, or one
//! that is cut short, nothing is included, and the model's player plays as the
//! robot does.

fn main() {
    let source = std::path::Path::new("src/ai/play_model.bin");
    println!("cargo:rerun-if-changed={}", source.display());
    let out = std::path::Path::new(&std::env::var("OUT_DIR").unwrap()).join("play_model.bin");
    let bytes = std::fs::read(source).unwrap_or_default();
    let model = if bytes.starts_with(b"PLAYMDL1") && bytes.len() >= 36 {
        let n = u32::from_le_bytes(bytes[8..12].try_into().unwrap()) as usize;
        match bytes.get(36..36 + 4 * n) {
            Some(weights) => [&bytes[8..24], weights].concat(),
            None => {
                println!(
                    "cargo:warning={} is cut short, so the robot is built without the play model",
                    source.display()
                );
                Vec::new()
            }
        }
    } else {
        if !bytes.is_empty() {
            println!(
                "cargo:warning={} is not a play model file, so the robot is built without the play model",
                source.display()
            );
        }
        Vec::new()
    };
    std::fs::write(out, model).unwrap();
}
