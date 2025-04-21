# Contributing to StartOS

This guide is for contributing to the StartOS. If you are interested in packaging a service for StartOS, visit the [service packaging guide](https://docs.start9.com/latest/developer-docs/). If you are interested in promoting, providing technical support, creating tutorials, or helping in other ways, please visit the [Start9 website](https://start9.com/contribute).

## Collaboration

- [Matrix](https://matrix.to/#/#community-dev:matrix.start9labs.com)
- [Telegram](https://t.me/start9_labs/47471)

## Project Structure

```bash
/
├── assets/
├── container-runtime/
├── core/
├── build/
├── debian/
├── web/
├── image-recipe/
├── patch-db
└── sdk/
```

#### assets

screenshots for the StartOS README

#### container-runtime

A NodeJS program that dynamically loads maintainer scripts and communicates with the OS to manage packages

#### core

An API, daemon (startd), and CLI (start-cli) that together provide the core functionality of StartOS.

#### build

Auxiliary files and scripts to include in deployed StartOS images

#### debian

Maintainer scripts for the StartOS Debian package

#### web

Web UIs served under various conditions and used to interact with StartOS APIs.

#### image-recipe

Scripts for building StartOS images

#### patch-db (submodule)

A diff based data store used to synchronize data between the web interfaces and server.

#### sdk

A typescript sdk for building start-os packages

## Environment Setup

#### Clone the StartOS repository

```sh
git clone https://github.com/Start9Labs/start-os.git --recurse-submodules
cd start-os
```

#### Continue to your project of interest for additional instructions:

- [`core`](core/README.md)
- [`web-interfaces`](web-interfaces/README.md)
- [`build`](build/README.md)
- [`patch-db`](https://github.com/Start9Labs/patch-db)

## Building

This project uses [GNU Make](https://www.gnu.org/software/make/) to build its components. To build any specific component, simply run `make <TARGET>` replacing `<TARGET>` with the name of the target you'd like to build

### Requirements

- [GNU Make](https://www.gnu.org/software/make/)
- [Docker](https://docs.docker.com/get-docker/)
- [NodeJS v20.16.0](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
- [sed](https://www.gnu.org/software/sed/)
- [grep](https://www.gnu.org/software/grep/)
- [awk](https://www.gnu.org/software/gawk/)
- [jq](https://jqlang.github.io/jq/)
- [gzip](https://www.gnu.org/software/gzip/)
- [brotli](https://github.com/google/brotli)

### Environment variables

- `PLATFORM`: which platform you would like to build for. Must be one of `x86_64`, `x86_64-nonfree`, `aarch64`, `aarch64-nonfree`, `raspberrypi`
  - NOTE: `nonfree` images are for including `nonfree` firmware packages in the built ISO
- `ENVIRONMENT`: a hyphen separated set of feature flags to enable
  - `dev`: enables password ssh (INSECURE!) and does not compress frontends
  - `unstable`: enables assertions that will cause errors on unexpected inconsistencies that are undesirable in production use either for performance or reliability reasons
  - `docker`: use `docker` instead of `podman`
- `GIT_BRANCH_AS_HASH`: set to `1` to use the current git branch name as the git hash so that the project does not need to be rebuilt on each commit

### Useful Make Targets

- `iso`: Create a full `.iso` image
  - Only possible from Debian
  - Not available for `PLATFORM=raspberrypi`
  - Additional Requirements:
    - [debspawn](https://github.com/lkhq/debspawn)
- `img`: Create a full `.img` image
  - Only possible from Debian
  - Only available for `PLATFORM=raspberrypi`
  - Additional Requirements:
    - [debspawn](https://github.com/lkhq/debspawn)
- `format`: Run automatic code formatting for the project
  - Additional Requirements:
    - [rust](https://rustup.rs/)
- `test`: Run automated tests for the project
  - Additional Requirements:
    - [rust](https://rustup.rs/)
- `update`: Deploy the current working project to a device over ssh as if through an over-the-air update
  - Requires an argument `REMOTE` which is the ssh address of the device, i.e. `start9@192.168.122.2`
- `reflash`: Deploy the current working project to a device over ssh as if using a live `iso` image to reflash it
  - Requires an argument `REMOTE` which is the ssh address of the device, i.e. `start9@192.168.122.2`
- `update-overlay`: Deploy the current working project to a device over ssh to the in-memory overlay without restarting it
  - WARNING: changes will be reverted after the device is rebooted
  - WARNING: changes to `init` will not take effect as the device is already initialized
  - Requires an argument `REMOTE` which is the ssh address of the device, i.e. `start9@192.168.122.2`
- `wormhole`: Deploy the `startbox` to a device using [magic-wormhole](https://github.com/magic-wormhole/magic-wormhole)
  - When the build it complete will emit a command to paste into the shell of the device to upgrade it
  - Additional Requirements:
    - [magic-wormhole](https://github.com/magic-wormhole/magic-wormhole)
- `clean`: Delete all compiled artifacts
