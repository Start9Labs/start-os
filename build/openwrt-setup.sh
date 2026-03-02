#!/bin/bash
set -eo pipefail

cd "$(git rev-parse --show-toplevel)"

echo "==> Copying feeds.conf to openwrt..."
cp build/feeds.conf openwrt/feeds.conf.default

echo "==> Updating feeds..."
cd openwrt
./scripts/feeds update -a

echo "==> Installing spacemit feeds..."
./scripts/feeds install -f -p spacemit_openwrt_feeds -a

echo "==> Installing remaining feeds..."
./scripts/feeds install -a

cd ..

echo "==> Copying diffconfig..."
cp build/openwrt.diffconfig openwrt/.config

echo "==> Expanding to full config..."
cd openwrt
make defconfig

echo "==> Downloading sources..."
make download V=s

echo "==> OpenWrt setup complete."
