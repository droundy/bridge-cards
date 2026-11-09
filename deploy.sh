#!/bin/bash

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..

# The booklets are embedded in the server, so make them from the current
# tables before building it.
cargo test -p robot --release write_booklets -- --ignored
if [ -n "$(git status --porcelain robot/booklets)" ]; then
    echo "WARNING: the booklets changed; commit robot/booklets so that main matches what is deployed" >&2
fi

cargo build -p abridge --release

cargo build -p abridge --release --target x86_64-unknown-linux-musl

strip target/x86_64-unknown-linux-musl/release/abridge

upx target/x86_64-unknown-linux-musl/release/abridge

scp target/x86_64-unknown-linux-musl/release/abridge droundy@abridgegame.com:abridge.new

ssh -l droundy abridgegame.com mv -v abridge.new abridge
