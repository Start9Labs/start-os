#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -ea
shopt -s expand_aliases

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi
if [ "$ARCH" = "arm64" ]; then
  ARCH="aarch64"
fi

if [ -z "$KERNEL_NAME" ]; then
	KERNEL_NAME=$(uname -s)
fi

if [ -z "$TARGET" ]; then
	if [ "$KERNEL_NAME" = "Linux" ]; then
		TARGET="$ARCH-unknown-linux-musl"
	elif [ "$KERNEL_NAME" = "Darwin" ]; then
		TARGET="$ARCH-apple-darwin"
	else
		>&2 echo "unknown kernel $KERNEL_NAME"
		exit 1
	fi
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

cd ..
FEATURES="$(echo $ENVIRONMENT | sed 's/-/,/g')"
RUSTFLAGS=""

if [[ "${ENVIRONMENT}" =~ (^|-)unstable($|-) ]]; then
	RUSTFLAGS="--cfg tokio_unstable"
fi

alias 'rust-zig-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$(pwd)":/home/rust/src -w /home/rust/src -P messense/cargo-zigbuild'

echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
rust-zig-builder sh -c "cd core && cargo zigbuild --release --no-default-features --features cli,daemon,$FEATURES --locked --bin start-cli --target=$TARGET"
if [ "$(ls -nd core/target/$TARGET/release/start-cli | awk '{ print $3 }')" != "$UID" ]; then
	rust-zig-builder sh -c "cd core && chown -R $UID:$UID target && chown -R $UID:$UID /root/.cargo"
fi