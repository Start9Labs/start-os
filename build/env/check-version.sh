#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

# The StartOS image version is the start-os crate version (the source of truth).
# Per-project versions are read from each project's manifest by basename.sh /
# dpkg-build.sh; this only materializes the OS image's /usr/lib/startos/VERSION.txt.
VERSION="$(grep -m1 '^version' ../../projects/start-os/Cargo.toml | sed -E 's/^version *= *"([^"]*)".*/\1/')"

if ! [ -f ./VERSION.txt ] || [ "$(cat ./VERSION.txt)" != "$VERSION" ]; then
    echo -n "$VERSION" > ./VERSION.txt
fi

echo -n ./build/env/VERSION.txt