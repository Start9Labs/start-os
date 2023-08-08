#!/bin/bash
# Reason for this being is that we need to create a snapshot for the deno runtime. It wants to pull 3 files from build, and during the creation it gets embedded, but for some 
# reason during the actual runtime it is looking for them. So this will create a docker in arm that creates the snaphot needed for the arm
set -e
shopt -s expand_aliases

if [ "$0" != "./build-arm-v8-snapshot.sh" ]; then
	>&2 echo "Must be run from backend/workspace directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-gnu-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$(pwd)":/home/rust/src -w /home/rust/src -P start9/rust-arm-cross:aarch64'

echo "Building "
cd ..
rust-gnu-builder sh -c "(cd libs/ && cargo build -p snapshot_creator --release --target=aarch64-unknown-linux-gnu)"
cd -

echo "Creating Arm v8 Snapshot"
docker run  $USE_TTY --platform linux/arm64/v8 --mount type=bind,src=$(pwd),dst=/mnt arm64v8/ubuntu:20.04 /bin/sh -c "cd /mnt && /mnt/target/aarch64-unknown-linux-gnu/release/snapshot_creator"
sudo chown -R $USER target
sudo chown -R $USER ~/.cargo
sudo chown $USER JS_SNAPSHOT.bin
sudo chmod 0644 JS_SNAPSHOT.bin
