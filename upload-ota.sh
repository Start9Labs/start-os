#!/bin/bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ -z "$TARGET" ]; then
    >&2 echo "TARGET is required"
    exit 1
fi

if [ -z "$KEY" ]; then
    >&2 echo "KEY is required"
    exit 1
fi

PLATFORM="$(cat ./PLATFORM.txt)"
VERSION="$(cat ./VERSION.txt)"
GIT_HASH="$(cat ./GIT_HASH.txt)"
if [[ "$GIT_HASH" =~ ^@ ]]; then
  GIT_HASH=unknown
else
  GIT_HASH="$(echo -n "$GIT_HASH" |  head -c 7)"
fi
STARTOS_ENV="$(cat ./ENVIRONMENT.txt)"
if [ -n "$STARTOS_ENV" ]; then
  GIT_HASH="$GIT_HASH~${STARTOS_ENV}"
fi

BASENAME="startos-${VERSION}-${GIT_HASH}_${PLATFORM}"

SHASUM=$(sha256sum results/$BASENAME.squashfs | awk '{print $1}')

curl -T results/${BASENAME}.squashfs "https://${TARGET}:8443/upload.cgi?key=${KEY}&gitHash=${GIT_HASH}&version=${VERSION}&platform=${PLATFORM}&shasum=${SHASUM}"