#!/bin/bash

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..

cargo build -p abridge --release

cargo build -p abridge --release --target x86_64-unknown-linux-musl

scp target/x86_64-unknown-linux-musl/release/abridge droundy@abridgegame.com:abridge.new

ssh -v -l droundy abridgegame.com mv -v abridge.new abridge
