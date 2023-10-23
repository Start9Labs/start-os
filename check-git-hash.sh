#!/bin/bash

if [ "$GIT_BRANCH_AS_HASH" != 1 ]; then
    GIT_HASH="$(git describe --always --abbrev=40 --dirty=-modified)"
else
    GIT_HASH="@$(git rev-parse --abbrev-ref HEAD)"
fi

if ! [ -f ./GIT_HASH.txt ] || [ "$(cat ./GIT_HASH.txt)" != "$GIT_HASH" ]; then
    echo -n "$GIT_HASH" > ./GIT_HASH.txt
fi

echo -n ./GIT_HASH.txt