#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-prod.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi

alias 'rust-arm64-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src -P start9/rust-arm-cross:aarch64'
build_cross () {
	cd ..
	if [[ "$FLAGS" = "" ]]; then
		rust-arm64-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release)"
	else
		echo "FLAGS=$FLAGS"
		rust-arm64-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release --features $FLAGS)"
	fi
	cd backend
}

build_arm64 () {
	if [[ "$FLAGS" = "" ]]; then
		cargo build --release
	else
		echo "bins=$EMBASSY_BINS"
		echo "FLAGS=$FLAGS"
		cargo build --release --features $FLAGS
	fi
	mkdir -p target/aarch64-unknown-linux-gnu/release/
	cp target/release/embassy* target/aarch64-unknown-linux-gnu/release/
}

if [[ "$(uname -m)" =~ ^(arm64|aarch64)$ ]]; then
	echo "Detected arm64 system, skipping cross-build"
    build_arm64
else
	echo "Detected non-arm64 system, cross-building"
	build_cross
fi


sudo chown -R $USER target
sudo chown -R $USER ~/.cargo

#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
