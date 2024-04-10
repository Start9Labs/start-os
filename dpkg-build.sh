#!/bin/bash

set -e

cd "$(dirname "${BASH_SOURCE[0]}")"

BASENAME=$(./basename.sh)
VERSION=$(cat ./VERSION.txt)
if [ "$PLATFORM" = "x86_64" ] || [ "$PLATFORM" = "x86_64-nonfree" ]; then
    DEB_ARCH=amd64
elif [ "$PLATFORM" = "aarch64" ] || [ "$PLATFORM" = "aarch64-nonfree" ] || [ "$PLATFORM" = "raspberrypi" ]; then
    DEB_ARCH=arm64
else
    DEB_ARCH="$PLATFORM"
fi

rm -rf dpkg-workdir/$BASENAME
mkdir -p dpkg-workdir/$BASENAME

make
make install DESTDIR=dpkg-workdir/$BASENAME

DEPENDS=$(cat dpkg-workdir/$BASENAME/usr/lib/startos/depends | tr $'\n' ',' | sed 's/,,\+/,/g' | sed 's/,$//')
CONFLICTS=$(cat dpkg-workdir/$BASENAME/usr/lib/startos/conflicts | tr $'\n' ',' | sed 's/,,\+/,/g' | sed 's/,$//')

cp -r debian dpkg-workdir/$BASENAME/DEBIAN
cat > dpkg-workdir/$BASENAME/DEBIAN/control << EOF
Package: startos
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
dpkg-deb --root-owner-group -b $BASENAME
mkdir -p ../results
mv $BASENAME.deb ../results/$BASENAME.deb
rm -rf $BASENAME