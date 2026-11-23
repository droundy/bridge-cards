#!/bin/bash

set -ev

./fetch-play-model.sh || echo "Building without the play model, whose robots then play as the others do."

cd robot
wasm-pack build --target web --no-typescript --release
cd ..
cargo run -p abridge --release