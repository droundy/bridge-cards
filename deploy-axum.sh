#!/bin/bash

# Like deploy.sh, but for abridge-axum.  The first time, install the units in
# systemd/ on the host (as root):
#
#   cp systemd/abridge-axum*.{service,path} /etc/systemd/system/
#   systemctl daemon-reload
#   systemctl enable --now abridge-axum-watcher.path
#
# After that, this script ships a new binary and abridge-axum-watcher.path
# restarts the service.  The first restart switches the site over from warp,
# since abridge-axum.service conflicts with abridge.service.  To switch back,
# run `systemctl start abridge.service`.

set -ev

cd robot
wasm-pack build --target web --no-typescript --release
cd ..

cargo build -p abridge --release --bin abridge-axum --target x86_64-unknown-linux-musl

strip target/x86_64-unknown-linux-musl/release/abridge-axum

upx target/x86_64-unknown-linux-musl/release/abridge-axum

scp target/x86_64-unknown-linux-musl/release/abridge-axum droundy@abridgegame.com:abridge-axum.new

ssh -l droundy abridgegame.com mv -v abridge-axum.new abridge-axum
