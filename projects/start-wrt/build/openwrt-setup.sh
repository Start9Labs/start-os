#!/bin/bash
set -eo pipefail

cd "$(git rev-parse --show-toplevel)"

echo "==> Copying feeds.conf to openwrt..."
cp build/feeds.conf openwrt/feeds.conf

echo "==> Updating feeds..."
cd openwrt
./scripts/feeds update -a

echo "==> Installing feeds..."
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
