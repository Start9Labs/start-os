#!/bin/bash
set -ea
shopt -s expand_aliases

cd "$(git rev-parse --show-toplevel)"

source build/builder-alias.sh

ARCH=${ARCH:-riscv64}
RUST_ARCH=${RUST_ARCH:-riscv64gc}
PROFILE=${PROFILE:-release}
if [ -z "$STARTWRT_GIT_HASH" ]; then
    STARTWRT_GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
    git diff-index --quiet HEAD -- 2>/dev/null || STARTWRT_GIT_HASH="${STARTWRT_GIT_HASH}-dirty"
fi
export STARTWRT_GIT_HASH

if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

# SpaceMiT K1 (BPI-F3) is RVA22 + V 1.0 + Zicbom/Zicbop/Zicboz — no RVA23.
# The cargo-zigbuild image bakes CARGO_TARGET_<triple>_RUSTFLAGS with an
# RVA23 profile (+zicond, +zfa, +zimop, +zcmop, +zvbb, +zvkt, etc.) that
# SIGILLs on K1. Use RUSTFLAGS (not the CARGO_TARGET_<triple> variant) —
# cargo-zigbuild reads RUSTFLAGS to select its linker wrapper, so setting it
# also makes it pick the clean generic_rv64 wrapper instead of the RVA23 one.
if [ "$RUST_ARCH" = "riscv64gc" ]; then
	# Clear the image's RVA23 default so our RUSTFLAGS takes effect
	# (CARGO_TARGET_<triple>_RUSTFLAGS outranks RUSTFLAGS when both are set).
	export CARGO_TARGET_RISCV64GC_UNKNOWN_LINUX_MUSL_RUSTFLAGS=""
	export RUSTFLAGS="-C target-cpu=generic-rv64 -C target-feature=+m,+a,+f,+d,+c,+v,+zicsr,+zifencei,+zicntr,+zihpm,+ziccif,+ziccamoa,+zicclsm,+ziccrse,+za64rs,+zihintpause,+zic64b,+zicbom,+zicbop,+zicboz,+zba,+zbb,+zbs,+zkt,+zihintntl,+zawrs"
	# C deps (aws-lc-sys, ring, openssl-sys, zstd-sys) go through our
	# K1-pinned zigcc wrapper, which bakes -mcpu into its zig cc invocation.
	# Do NOT also set CFLAGS_<target>: aws-lc-sys's jitter-entropy sub-build
	# appends `-Wp,-U_FORTIFY_SOURCE` only when a user CFLAGS is present, and
	# `zig cc` rejects `-Wp,` passthrough flags.
	export CC_riscv64gc_unknown_linux_musl=/workdir/build/zigcc-k1.sh
	export CXX_riscv64gc_unknown_linux_musl=/workdir/build/zigcxx-k1.sh
	export CARGO_TARGET_RISCV64GC_UNKNOWN_LINUX_MUSL_LINKER=/workdir/build/zigcc-k1.sh
fi

# start-os's build.rs reads build/env/GIT_HASH.txt; generate it first so the
# start-os path dependency compiles from a clean submodule checkout.
if [ -f start-os/build/env/check-git-hash.sh ]; then
    (cd start-os && bash build/env/check-git-hash.sh >/dev/null)
fi

echo "==> Building Rust binaries (arch=$RUST_ARCH, profile=$PROFILE)..."
rust-zig-builder cargo zigbuild \
    --manifest-path=./backend/ctrl/Cargo.toml \
    $BUILD_FLAGS \
    --locked \
    --target=${RUST_ARCH}-unknown-linux-musl

# Fix ownership if built in Docker as root
if [ "$(ls -nd "backend/target/$RUST_ARCH-unknown-linux-musl/$PROFILE/startwrt" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "chown -R $UID:$UID backend/target && chown -R $UID:$UID /usr/local/cargo"
fi

# Verify the binary doesn't use any RVA23-only instructions that would
# SIGILL on the SpaceMiT K1 (BPI-F3) target.
if [ "$RUST_ARCH" = "riscv64gc" ]; then
    bash build/verify-isa.sh "backend/target/${RUST_ARCH}-unknown-linux-musl/${PROFILE}/startwrt"
fi

echo "==> Rust build complete."
