#!/bin/bash

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..
cargo run -p abridge --release