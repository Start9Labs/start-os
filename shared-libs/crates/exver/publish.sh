#!/bin/sh

set -e

cd `dirname "$BASH_SOURCE"`
rm -rf pkg
wasm-pack build --release --target=bundler -- --features=wasm-bindgen
cp exver.d.ts pkg/exver.d.ts
cd pkg
jq '.name = "@start9labs/exver"' package.json > package.json.tmp
mv package.json.tmp package.json
npm publish