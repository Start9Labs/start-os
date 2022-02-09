#!/bin/bash

cd "$(dirname "$0")"

TMP_FILE=$(mktemp)

jq ".gitHash = \"$(git rev-parse HEAD)\"" config.json > $TMP_FILE && mv $TMP_FILE config.json
