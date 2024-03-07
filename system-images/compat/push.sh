#!/bin/bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

ln -sf x86_64-unknown-linux-musl target/amd64
ln -sf aarch64-unknown-linux-musl target/arm64
docker buildx build --tag start9/compat --platform=linux/arm64,linux/amd64 --push .