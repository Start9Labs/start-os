#!/bin/bash

set -e
shopt -s expand_aliases

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

if [ "$0" != "./build-prod.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-gnu-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src -P start9/rust-arm-cross:aarch64'
alias 'rust-musl-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src -it -P messense/rust-musl-cross:$ARCH-musl'

cd ..
FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi
if [[ "$FLAGS" = "" ]]; then
	rust-gnu-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release --locked  --target=$ARCH-unknown-linux-gnu)"
	rust-musl-builder sh -c "(git config --global --add safe.directory '*'; cd libs && cargo build --release --locked --bin embassy_container_init  --target=$ARCH-unknown-linux-gnu)"
else
	echo "FLAGS=$FLAGS"
	rust-gnu-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release --features $FLAGS --locked --target=$ARCH-unknown-linux-gnu)"
	rust-musl-builder sh -c "(git config --global --add safe.directory '*'; cd libs && cargo build --release --features $FLAGS --locked --bin embassy_container_init  --target=$ARCH-unknown-linux-gnu)"
fi
cd backend

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo
sudo chown -R $USER ../libs/target

#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
