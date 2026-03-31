#!/bin/bash
# Save Docker images needed by the 0.3.6-alpha.0 migration as tarballs
# so they can be bundled into the OS and loaded without internet access.
set -e

ARCH="${ARCH:-x86_64}"
DESTDIR="${1:-build/lib/migration-images}"

if [ "$ARCH" = "x86_64" ]; then
    DOCKER_PLATFORM="linux/amd64"
elif [ "$ARCH" = "aarch64" ]; then
    DOCKER_PLATFORM="linux/arm64"
else
    DOCKER_PLATFORM="linux/$ARCH"
fi

IMAGES=("tonistiigi/binfmt:latest")
if [ "$ARCH" != "riscv64" ]; then
    IMAGES=("start9/compat:latest" "start9/utils:latest" "${IMAGES[@]}")
fi

mkdir -p "$DESTDIR"

for IMAGE in "${IMAGES[@]}"; do
    FILENAME=$(echo "$IMAGE" | sed 's|/|_|g; s/:/_/g').tar
    if [ -f "$DESTDIR/$FILENAME" ]; then
        echo "Skipping $IMAGE (already saved)"
        continue
    fi
    echo "Pulling $IMAGE for $DOCKER_PLATFORM..."
    docker pull --platform "$DOCKER_PLATFORM" "$IMAGE"
    echo "Saving $IMAGE to $DESTDIR/$FILENAME..."
    docker save "$IMAGE" -o "$DESTDIR/$FILENAME"
done

echo "Migration images saved to $DESTDIR"
