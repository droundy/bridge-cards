//! Puts the play model where the build includes it, from the play model file
//! `src/ai/play_model.bin`, which is not kept in git (see `fetch-play-model.sh`
//! and `src/ai/fitted_play/model_file.rs`).  What is included is the game's
//! model as the file holds it: the length of the name of its shape as a
//! little-endian `u32`, that name, the number of its weights as a `u32`, and
//! the weights as `f32`s.  In a file of the second version (`PLAYMDL2`) that
//! is what follows the round and the numbers of learners and past models.  A
//! file of the first version (`PLAYMDL1`) has only the shape `v1`, and its
//! header gives the number of weights, which follow after 36 bytes.  With no
//! file, or one that is cut short, nothing is included, and the model's
//! player plays as the robot does.

fn main() {
    let source = std::path::Path::new("src/ai/play_model.bin");
    println!("cargo:rerun-if-changed={}", source.display());
    let out = std::path::Path::new(&std::env::var("OUT_DIR").unwrap()).join("play_model.bin");
    let bytes = std::fs::read(source).unwrap_or_default();
    let number = |at: usize| -> Option<usize> {
        Some(u32::from_le_bytes(bytes.get(at..at + 4)?.try_into().unwrap()) as usize)
    };
    let model = if bytes.starts_with(b"PLAYMDL2") {
        (|| {
            let length = number(20)?;
            let n = number(24 + length)?;
            Some(bytes.get(20..28 + length + 4 * n)?.to_vec())
        })()
    } else if bytes.starts_with(b"PLAYMDL1") {
        (|| {
            let n = number(8)?;
            let weights = bytes.get(36..36 + 4 * n)?;
            Some([&2u32.to_le_bytes()[..], b"v1", &(n as u32).to_le_bytes(), weights].concat())
        })()
    } else {
        if !bytes.is_empty() {
            println!(
                "cargo:warning={} is not a play model file, so the robot is built without the play model",
                source.display()
            );
        }
        Some(Vec::new())
    };
    let model = model.unwrap_or_else(|| {
        println!(
            "cargo:warning={} is cut short, so the robot is built without the play model",
            source.display()
        );
        Vec::new()
    });
    std::fs::write(out, model).unwrap();
}
