#!/bin/bash
set -e

DEB_PATH="$(realpath $1)"

cd "$(dirname "${BASH_SOURCE[0]}")"/..

BASEDIR="$(pwd -P)"

VERSION="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/VERSION.txt)"
GIT_HASH="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/GIT_HASH.txt)"
if [[ "$GIT_HASH" =~ ^@ ]]; then
  GIT_HASH="unknown"
else
  GIT_HASH="$(echo -n "$GIT_HASH" |  head -c 7)"
fi
STARTOS_ENV="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/ENVIRONMENT.txt)"
PLATFORM="$(dpkg-deb --fsys-tarfile $DEB_PATH | tar --to-stdout -xvf - ./usr/lib/startos/PLATFORM.txt)"

if [ "$PLATFORM" = "x86_64" ] || [ "$PLATFORM" = "x86_64-nonfree" ]; then
	ARCH=amd64
	QEMU_ARCH=x86_64
elif [ "$PLATFORM" = "aarch64" ] || [ "$PLATFORM" = "aarch64-nonfree" ] || [ "$PLATFORM" = "raspberrypi" ]  || [ "$PLATFORM" = "rockchip64" ]; then
	ARCH=arm64
	QEMU_ARCH=aarch64
else
	ARCH="$PLATFORM"
	QEMU_ARCH="$PLATFORM"
fi

SUITE=bookworm

debspawn list | grep $SUITE || debspawn create $SUITE

VERSION_FULL="${VERSION}-${GIT_HASH}"
if [ -n "$STARTOS_ENV" ]; then
  VERSION_FULL="$VERSION_FULL~${STARTOS_ENV}"
fi

if [ -z "$DSNAME" ]; then
	DSNAME="$SUITE"
fi

if [ "$QEMU_ARCH" != "$(uname -m)" ]; then
  sudo update-binfmts --import qemu-$QEMU_ARCH
fi

imgbuild_fname="$(mktemp /tmp/exec-mkimage.XXXXXX)"
cat > $imgbuild_fname <<END
#!/bin/sh

export IB_SUITE=${SUITE}
export IB_TARGET_ARCH=${ARCH}
export IB_TARGET_PLATFORM=${PLATFORM}
export IB_OS_ENV=${STARTOS_ENV}
export VERSION=${VERSION}
export VERSION_FULL=${VERSION_FULL}
exec ./build.sh
END

prepare_hash=$(sha1sum ${BASEDIR}/image-recipe/prepare.sh | head -c 7)

mkdir -p ${BASEDIR}/image-recipe/deb
cp $DEB_PATH ${BASEDIR}/image-recipe/deb/

mkdir -p ${BASEDIR}/results
set +e
debspawn run \
	-x \
	--allow=read-kmods,kvm,full-dev \
	--cachekey="${SUITE}-${prepare_hash}-mkimage" \
	--init-command="${BASEDIR}/image-recipe/prepare.sh" \
	--build-dir="${BASEDIR}/image-recipe" \
	--artifacts-out="${BASEDIR}/results" \
	--header="StartOS Image Build" \
	--suite=${SUITE} \
	${DSNAME} \
	${imgbuild_fname}

retval=$?
rm $imgbuild_fname
if [ $retval -ne 0 ]; then
    exit $retval
fi
exit 0
