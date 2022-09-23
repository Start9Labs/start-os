#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-prod-native.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

FLAGS=""
if [[ "$ENVIRONMENT" =~ (^|-)unstable($|-) ]]; then
	FLAGS="unstable,$FLAGS"
fi
if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	FLAGS="dev,$FLAGS"
fi
if [[ "$FLAGS" = "" ]]; then
	cargo +stable build --release --locked
else
	echo "FLAGS=$FLAGS"
	cargo +stable build --release --features $FLAGS --locked
fi

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo

#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
