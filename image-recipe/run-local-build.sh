#!/bin/bash
set -e

cd "$(dirname "${BASH_SOURCE[0]}")"/..

BASEDIR="$(pwd -P)"

SUITE=trixie

dockerfile_hash=$(sha256sum ${BASEDIR}/image-recipe/prepare.sh | head -c 7)

docker_img_name="startos_build:${SUITE}-${dockerfile_hash}"

if [ -z "$(docker images -q "${docker_img_name}")" ]; then
	docker build --build-arg=SUITE=${SUITE} -t "${docker_img_name}" ./image-recipe
fi

docker run $USE_TTY --rm --privileged -v "$(pwd)/image-recipe:/root/image-recipe" -v "$(pwd)/results:/root/results" \
	-e IB_SUITE="$SUITE" \
	-e IB_UID="$UID" \
	-e IB_INCLUDE \
	"${docker_img_name}" /root/image-recipe/build.sh $@
