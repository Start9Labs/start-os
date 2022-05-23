#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-prod.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

alias 'rust-arm64-builder'='docker run --rm -it -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-arm-cross:aarch64'

cd ..
FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)beta($|-) ]]; then
	FLAGS="beta,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi
if [[ "$FLAGS" = "" ]]; then
	rust-arm64-builder sh -c "(cd backend && cargo build --release)"
else
	echo "FLAGS=$FLAGS"
	rust-arm64-builder sh -c "(cd backend && cargo build --release --features $FLAGS)"
fi
cd backend
#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
