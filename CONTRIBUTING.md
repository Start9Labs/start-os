# Contributing to StartOS

This guide is for contributing to the StartOS. If you are interested in packaging a service for StartOS, visit the [service packaging guide](https://docs.start9.com/latest/developer-docs/). If you are interested in promoting, providing technical support, creating tutorials, or helping in other ways, please visit the [Start9 website](https://start9.com/contributing).

## Collaboration

- [Matrix](https://matrix.to/#/#community-dev:matrix.start9labs.com)
- [Telegram](https://t.me/start9_labs/47471)

## Project Structure

```bash
/
├── assets/
├── core/
├── build/
├── debian/
├── web/
├── image-recipe/
├── libs/
├── patch-db
└── system-images/
```
#### assets
screenshots for the StartOS README

#### core
An API, daemon (startd), CLI (start-cli), and SDK (start-sdk) that together provide the core functionality of StartOS.

#### build
Scripts for building StartOS

#### debian
Necessary for building debian images

#### web
Web UIs served under various conditions and used to interact with StartOS APIs.

#### image-recipe
Scripts for building StartOS images

#### libs
A set of standalone crates that were separated out of `backend` for the purpose of portability.

#### patch-db (submodule)
A diff based data store used to synchronize data between the web interfaces and server.

#### system-images
Docker images that assist with creating backups.

## Environment Setup

#### Clone the StartOS repository
```sh
git clone https://github.com/Start9Labs/start-os.git
cd start-os
```

#### Load the PatchDB submodule
```sh
git submodule update --init --recursive
```

#### Continue to your project of interest for additional instructions:
- [`core`](core/README.md)
- [`web-interfaces`](web-interfaces/README.md)
- [`build`](build/README.md)
- [`patch-db`](https://github.com/Start9Labs/patch-db)
