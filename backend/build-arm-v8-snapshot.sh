#!/bin/bash
# Reason for this being is that we need to create a snapshot for the deno runtime. It wants to pull 3 files from build, and during the creation it gets embedded, but for some 
# reason during the actual runtime it is looking for them. So this will create a docker in arm that creates the snaphot needed for the arm
set -e

if [ "$0" != "./build-arm-v8-snapshot.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

echo "Creating Arm v8 Snapshot"
docker run --platform linux/arm64/v8 --mount type=bind,src=$(pwd),dst=/mnt arm64v8/ubuntu:20.04 /bin/sh -c "cd /mnt && /mnt/target/aarch64-unknown-linux-gnu/release/create-js-snapshots"
mv JS_SNAPSHOT.bin src/procedure/js_scripts/ARM_JS_SNAPSHOT.bin