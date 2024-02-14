#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

cat ./package.json | sed 's/file:\.\([.\/]\)/file:..\/.\1/g' > ./dist/package.json
cat ./package-lock.json | sed 's/"\.\([.\/]\)/"..\/.\1/g' > ./dist/package-lock.json

npm --prefix dist ci --omit=dev