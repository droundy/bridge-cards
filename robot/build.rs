//! Puts the weights of the play model (`src/ai/play_model.bin`, which is not
//! kept in git) where the build includes them, or nothing when there are
//! none, in which case the model's player plays as the robot does.

fn main() {
    let source = std::path::Path::new("src/ai/play_model.bin");
    println!("cargo:rerun-if-changed={}", source.display());
    let out = std::path::Path::new(&std::env::var("OUT_DIR").unwrap()).join("play_model.bin");
    let bytes = std::fs::read(source).unwrap_or_default();
    std::fs::write(out, bytes).unwrap();
}
