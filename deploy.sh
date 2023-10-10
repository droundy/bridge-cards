#!/bin/bash

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..

cargo build -p abridge --release

cargo build -p abridge --release --target x86_64-unknown-linux-musl

strip target/x86_64-unknown-linux-musl/release/abridge

upx target/x86_64-unknown-linux-musl/release/abridge

scp target/x86_64-unknown-linux-musl/release/abridge droundy@abridgegame.com:abridge.new

ssh -l droundy abridgegame.com mv -v abridge.new abridge
