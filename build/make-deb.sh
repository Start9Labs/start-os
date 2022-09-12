#!/bin/bash

set -e

rm -rf debian && mkdir debian
cp -R DEBIAN debian/

mkdir -p debian/etc/embassy
cp ENVIRONMENT.txt debian/etc/embassy/
cp GIT_HASH.txt debian/etc/embassy/

mkdir -p debian/usr/local/bin
cp backend/target/aarch64-unknown-linux-gnu/release/embassy-init debian/usr/local/bin/
cp backend/target/aarch64-unknown-linux-gnu/release/embassyd debian/usr/local/bin/
cp backend/target/aarch64-unknown-linux-gnu/release/embassy-cli debian/usr/local/bin/

mkdir -p debian/etc/systemd/system
cp backend/embassy-init.service debian/etc/systemd/system/
cp backend/embassyd.service debian/etc/systemd/system/

mkdir -p debian/var/lib/embassy/system-images
cp system-images/**/*.tar debian/var/lib/embassy/system-images/

mkdir -p debian/var/www/html
cp -r frontend/dist/diagnostic-ui debian/var/www/html/diagnostic
cp -r frontend/dist/setup-wizard debian/var/www/html/setup
cp -r frontend/dist/ui debian/var/www/html/main
cp index.html debian/var/www/html/

dpkg-deb --root-owner-group --build debian embassy-os.deb