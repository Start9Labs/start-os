#!/bin/bash
set -e

cd "$(dirname "${BASH_SOURCE[0]}")/../.."

BASEDIR="$(pwd -P)"

SUITE=trixie

USE_TTY=
if tty -s; then
  USE_TTY="-it"
fi

dockerfile_hash=$(sha256sum ${BASEDIR}/build/image-recipe/Dockerfile | head -c 7)

docker_img_name="start9/build-iso:${SUITE}-${dockerfile_hash}"

platform=linux/${ARCH}
case $ARCH in
	x86_64)
		platform=linux/amd64;;
	aarch64)
		platform=linux/arm64;;
esac

if ! docker run --rm --platform=$platform "${docker_img_name}" true 2> /dev/null; then
	docker buildx build --load --platform=$platform --build-arg=SUITE=${SUITE} -t "${docker_img_name}" ./build/image-recipe
fi

docker run $USE_TTY --rm --platform=$platform --privileged -v "$(pwd)/build/image-recipe:/root/image-recipe" -v "$(pwd)/results:/root/results" \
	-e IB_SUITE="$SUITE" \
	-e IB_UID="$UID" \
	-e IB_INCLUDE \
	"${docker_img_name}" /root/image-recipe/build.sh $@
