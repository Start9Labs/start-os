#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -ea
shopt -s expand_aliases

PROFILE=${PROFILE:-release}
if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

if [ "$ARCH" = "arm64" ]; then
	ARCH="aarch64"
fi

RUST_ARCH="$ARCH"
if [ "$ARCH" = "riscv64" ]; then
	RUST_ARCH="riscv64gc"
fi

cd ..

rm -rf core/startos/bindings/

FEATURES="$(echo $ENVIRONMENT | sed 's/-/,/g')"
RUSTFLAGS=""
if [[ "${ENVIRONMENT}" =~ (^|-)console($|-) ]]; then
	RUSTFLAGS="--cfg tokio_unstable"
fi
echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
cross test --manifest-path=./core/Cargo.toml $BUILD_FLAGS --no-default-features --features test,$FEATURES --locked 'export_bindings_'

cd core/startos/bindings

for folder in $(find . -type d); do
	(
		cd $folder
		find . -name '*.ts' -maxdepth 1 | sed 's/\.\/\([^.]*\)\.ts/export * from ".\/\1";/g' | grep -v '"./index"' | tee ./index.ts
		find . -mindepth 1 -maxdepth 1 -type d | sed 's/\.\/\([^.]*\)/export * as \1 from ".\/\1";/g' | grep -v '"./index"' | tee -a ./index.ts
	)
done

find . -name '*.ts' | xargs -P $(nproc) -I % npm --prefix ../../../sdk exec -- prettier --config ../../../sdk/base/package.json -w %
