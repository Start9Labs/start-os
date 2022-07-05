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

alias 'rust-arm64-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src -P start9/rust-arm-cross:aarch64'

cd ..
FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi
if [[ "$FLAGS" = "" ]]; then
	rust-arm64-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release)"
else
	echo "FLAGS=$FLAGS"
	rust-arm64-builder sh -c "(git config --global --add safe.directory '*'; cd backend && cargo build --release --features $FLAGS)"
fi
cd backend
#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
