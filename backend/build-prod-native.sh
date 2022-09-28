#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-prod-native.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-native-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$(pwd)":/root -P start9/rust'

cd ..
FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi
if [[ "$FLAGS" = "" ]]; then
	rust-native-builder sh -c "cd ~/backend && cargo build --release --locked"
else
	echo "FLAGS=$FLAGS"
	rust-native-builder sh -c "cd ~/backend && cargo build --release --features $FLAGS --locked"
fi
cd backend

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo

#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
