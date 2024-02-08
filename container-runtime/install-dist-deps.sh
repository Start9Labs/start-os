#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
    rsync -a --copy-unsafe-links node_modules/ dist/node_modules/
else
    npm --prefix dist ci --omit=dev
fi