#!/bin/bash

cd "$(dirname ${BASH_SOURCE[0]})"

set -e

if [[ "$(cat ../build/env/ENVIRONMENT.txt)" =~ (^|-)beta($|-) ]]; then
    BETA='beta-'
fi

CONFIG=$(if [ -f config.json ]; then cat config.json; else cat config-sample.json; fi)
echo "$CONFIG" \
    | jq '.useMocks = false' \
    | jq ".gitHash = \"$(cat ../build/env/GIT_HASH.txt)\"" \
    | jq ".defaultRegistry = \"https://${BETA}registry.start9.com\"" \
    > config.json