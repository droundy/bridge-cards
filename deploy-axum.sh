#!/bin/bash

# Like deploy.sh, but ships abridge-axum.  It goes to the host as `abridge`, in
# place of the warp binary, so the existing abridge.service and
# abridge-watcher.path restart it with the same arguments, and the name won't
# change when abridge-axum is renamed to abridge.  Run deploy.sh to put the
# warp one back.
#
# The certificate comes from a new cache, ./acme-cache in the service's working
# directory, so the first start asks Let's Encrypt for one, and https doesn't
# work until it has it.

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..

cargo build -p abridge --release --bin abridge-axum --target x86_64-unknown-linux-musl

strip target/x86_64-unknown-linux-musl/release/abridge-axum

upx target/x86_64-unknown-linux-musl/release/abridge-axum

scp target/x86_64-unknown-linux-musl/release/abridge-axum droundy@abridgegame.com:abridge.new

ssh -l droundy abridgegame.com mv -v abridge.new abridge
