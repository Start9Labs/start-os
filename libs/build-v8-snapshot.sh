#!/bin/bash
# Reason for this being is that we need to create a snapshot for the deno runtime. It wants to pull 3 files from build, and during the creation it gets embedded, but for some 
# reason during the actual runtime it is looking for them. So this will create a docker in arm that creates the snaphot needed for the arm
set -e

if [ "$0" != "./build-v8-snapshot.sh" ]; then
	>&2 echo "Must be run from backend/workspace directory"
	exit 1
fi

echo "Creating v8 Snapshot"
cargo run -p snapshot-creator --release
sudo chown ${whoami}:${whoami} JS_SNAPSHOT.bin
sudo chmod 0644 JS_SNAPSHOT.bin

sudo mv -f JS_SNAPSHOT.bin ./js_engine/src/artifacts/JS_SNAPSHOT.bin