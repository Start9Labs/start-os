# Release Process

## `embassyos_0.3.x-1_amd64.deb`

- Description: debian package for x86_64 - intended to be installed on pureos
- Destination: GitHub Release Tag
- Requires: N/A
- Build steps:
  - Clone `https://github.com/Start9Labs/embassy-os-deb` at `master`
  - Run `make TAG=master` from that folder
- Artifact: `./embassyos_0.3.x-1_amd64.deb`

## `eos-<version>-<git hash>-<date>_amd64.iso`

- Description: live usb image for x86_64
- Destination: GitHub Release Tag
- Requires: `embassyos_0.3.x-1_amd64.deb`
- Build steps:
  - Clone `https://github.com/Start9Labs/eos-image-recipes` at `master`
  - Copy `embassyos_0.3.x-1_amd64.deb` to
    `overlays/vendor/root/embassyos_0.3.x-1_amd64.deb`
  - Run `./run-local-build.sh byzantium` from that folder
- Artifact: `./results/eos-<version>-<git hash>-<date>_amd64.iso`

## `eos.x86_64.squashfs`

- Description: compressed embassyOS x86_64 filesystem image
- Destination: GitHub Release Tag, Registry @
  `resources/eos/<version>/eos.x86_64.squashfs`
- Requires: `eos-<version>-<git hash>-<date>_amd64.iso`
- Build steps:
  - From `https://github.com/Start9Labs/eos-image-recipes` at `master`
  - `./extract-squashfs.sh results/eos-<version>-<git hash>-<date>_amd64.iso`
- Artifact: `./results/eos.x86_64.squashfs`

## `eos.raspberrypi.squashfs`

- Description: compressed embassyOS raspberrypi filesystem image
- Destination: GitHub Release Tag, Registry @
  `resources/eos/<version>/eos.raspberrypi.squashfs`
- Requires: N/A
- Build steps:
  - Clone `https://github.com/Start9Labs/embassy-os` at `master`
  - `make embassyos-raspi.img`
  - flash `embassyos-raspi.img` to raspberry pi
  - boot raspberry pi with ethernet
  - wait for chime
    - you can watch logs using `nc <ip> 8080`
  - unplug raspberry pi, put sd card back in build machine
  - `./build/raspberry-pi/rip-image.sh`
- Artifact: `./eos.raspberrypi.squashfs`

## `lite-upgrade.img`

- Description: update image for users coming from 0.3.2.1 and before
- Destination: Registry @ `resources/eos/<version>/eos.img`
- Requires: `eos.raspberrypi.squashfs`
- Build steps:
  - From `https://github.com/Start9Labs/embassy-os` at `master`
  - `make lite-upgrade.img`
- Artifact `./lite-upgrade.img`

## `eos-<version>-<git hash>-<date>_raspberrypi.tar.gz`

- Description: pre-initialized raspberrypi image
- Destination: GitHub Release Tag (as tar.gz)
- Requires: `eos.raspberrypi.squashfs`
- Build steps:
  - From `https://github.com/Start9Labs/embassy-os` at `master`
  - `make eos_raspberrypi.img`
  - `tar --format=posix -cS -f- eos-<version>-<git hash>-<date>_raspberrypi.img | gzip > eos-<version>-<git hash>-<date>_raspberrypi.tar.gz`
- Artifact `./eos-<version>-<git hash>-<date>_raspberrypi.tar.gz`
