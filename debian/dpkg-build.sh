#!/bin/bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")/.."

PROJECT=${PROJECT:-"startos"}
BASENAME=${BASENAME:-"$(./build/env/basename.sh)"}
VERSION=${VERSION:-$(cat ./build/env/VERSION.txt)}
if [ "$PLATFORM" = "x86_64" ] || [ "$PLATFORM" = "x86_64-nonfree" ] || [ "$PLATFORM" = "x86_64-nvidia" ]; then
    DEB_ARCH=amd64
elif [ "$PLATFORM" = "aarch64" ] || [ "$PLATFORM" = "aarch64-nonfree" ] || [ "$PLATFORM" = "aarch64-nvidia" ] || [ "$PLATFORM" = "raspberrypi" ]; then
    DEB_ARCH=arm64
else
    DEB_ARCH="$PLATFORM"
fi

rm -rf dpkg-workdir/$BASENAME
mkdir -p dpkg-workdir/$BASENAME

if [ "${PROJECT}" = "startos" ]; then
    INSTALL_TARGET="install"
else
    INSTALL_TARGET="install-${PROJECT#start-}"
fi
make "${INSTALL_TARGET}" DESTDIR=dpkg-workdir/$BASENAME REMOTE=

if [ -f dpkg-workdir/$BASENAME/usr/lib/$PROJECT/depends ]; then
    if [ -n "$DEPENDS" ]; then
        DEPENDS="$DEPENDS,"
    fi
    DEPENDS="${DEPENDS}$(cat dpkg-workdir/$BASENAME/usr/lib/$PROJECT/depends | tr $'\n' ',' | sed 's/,,\+/,/g' | sed 's/,$//')"
fi
if [ -f dpkg-workdir/$BASENAME/usr/lib/$PROJECT/conflicts ]; then
    if [ -n "$CONFLICTS" ]; then
        CONFLICTS="$CONFLICTS,"
    fi
    CONFLICTS="${CONFLICTS}$(cat dpkg-workdir/$BASENAME/usr/lib/$PROJECT/conflicts | tr $'\n' ',' | sed 's/,,\+/,/g' | sed 's/,$//')"
fi
CONFLICTS=${CONFLICTS:-"$(cat dpkg-workdir/$BASENAME/usr/lib/startos/conflicts | tr $'\n' ',' | sed 's/,,\+/,/g' | sed 's/,$//')"}

if [ -d debian/${PROJECT} ]; then
    cp -r debian/${PROJECT} dpkg-workdir/$BASENAME/DEBIAN
else
    mkdir -p dpkg-workdir/$BASENAME/DEBIAN
fi
cat > dpkg-workdir/$BASENAME/DEBIAN/control << EOF
Package: ${PROJECT}
Version: ${VERSION}
Section: unknown
Priority: required
Maintainer: Aiden McClelland <aiden@start9.com>
Homepage: https://start9.com
Architecture: ${DEB_ARCH}
Multi-Arch: foreign
Depends: ${DEPENDS}
Conflicts: ${CONFLICTS}
Description: StartOS Debian Package
EOF

cd dpkg-workdir/$BASENAME
find . -type f -not -path "./DEBIAN/*" -exec md5sum {} \; | sort -k 2 | sed 's/\.\/\(.*\)/\1/' > DEBIAN/md5sums
cd ../..

cd dpkg-workdir
dpkg-deb --root-owner-group -Zzstd -b $BASENAME
mkdir -p ../results
mv $BASENAME.deb ../results/$BASENAME.deb
rm -rf $BASENAME