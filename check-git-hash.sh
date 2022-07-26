#!/bin/bash

GIT_HASH="$(git describe --always --abbrev=40 --dirty=-modified)"

if ! [ -f ./GIT_HASH.txt ] || [ "$(cat ./GIT_HASH.txt)" != "$GIT_HASH" ]; then
    echo -n "$GIT_HASH" > ./GIT_HASH.txt
fi

echo -n ./GIT_HASH.txt