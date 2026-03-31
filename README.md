<div align="center">
  <img src="web/projects/shared/assets/img/icon.png" alt="StartOS Logo" width="16%" />
  <h1 style="margin-top: 0;">StartOS</h1>
  <a href="https://github.com/Start9Labs/start-os/releases">
    <img alt="GitHub release (with filter)" src="https://img.shields.io/github/v/release/start9labs/start-os?logo=github">
  </a>
  <a href="https://github.com/Start9Labs/start-os/actions/workflows/startos-iso.yaml">
    <img src="https://github.com/Start9Labs/start-os/actions/workflows/startos-iso.yaml/badge.svg">
  </a>
  <a href="https://heyapollo.com/product/startos">
    <img alt="Static Badge" src="https://img.shields.io/badge/apollo-review%20%E2%AD%90%E2%AD%90%E2%AD%90%E2%AD%90%E2%AD%90%20-slateblue">
  </a>
  <a href="https://twitter.com/start9labs">
    <img alt="X (formerly Twitter) Follow" src="https://img.shields.io/twitter/follow/start9labs">
  </a>
  <a href="https://docs.start9.com">
    <img alt="Static Badge" src="https://img.shields.io/badge/docs-orange?label=%F0%9F%91%A4%20support">
  </a>
  <a href="https://matrix.to/#/#dev-startos:matrix.start9labs.com">
    <img alt="Static Badge" src="https://img.shields.io/badge/developer-matrix-darkcyan?logo=matrix">
  </a>
  <a href="https://start9.com">
    <img alt="Website" src="https://img.shields.io/website?up_message=online&down_message=offline&url=https%3A%2F%2Fstart9.com&logo=website&label=%F0%9F%8C%90%20website">
  </a>
</div>

## What is StartOS?

StartOS is an open-source Linux distribution for running a personal server. It handles discovery, installation, network configuration, data backup, dependency management, and health monitoring of self-hosted services.

**Tech stack:** Rust backend (Tokio/Axum), Angular frontend, Node.js container runtime with LXC, and a custom diff-based database ([Patch-DB](https://github.com/Start9Labs/patch-db)) for reactive state synchronization.

Services run in isolated LXC containers, packaged as [S9PKs](https://github.com/Start9Labs/start-os/blob/master/core/s9pk-structure.md) — a signed, merkle-archived format that supports partial downloads and cryptographic verification.

## What can you do with it?

StartOS lets you self-host services that would otherwise depend on third-party cloud providers — giving you full ownership of your data and infrastructure.

Browse available services on the [Start9 Marketplace](https://marketplace.start9.com/), including:

- **Bitcoin & Lightning** — Run a full Bitcoin node, Lightning node, BTCPay Server, and other payment infrastructure
- **Communication** — Self-host Matrix, SimpleX, or other messaging platforms
- **Cloud Storage** — Run Nextcloud, Vaultwarden, and other productivity tools

Services are added by the community. If a service you want isn't available, you can [package it yourself](https://github.com/Start9Labs/ai-service-packaging/).

## Getting StartOS

### Buy a Start9 server

The easiest path. [Buy a server](https://store.start9.com) from Start9 and plug it in.

### Build your own

Follow the [install guide](https://docs.start9.com/start-os/installing.html) to install StartOS on your own hardware. . Reasons to go this route:

1. You already have compatible hardware
2. You want to save on shipping costs
3. You prefer not to share your physical address
4. You enjoy building things

### Build from source

See [CONTRIBUTING.md](CONTRIBUTING.md) for environment setup, build instructions, and development workflow.

## Contributing

There are multiple ways to contribute: work directly on StartOS, package a service for the marketplace, or help with documentation and guides. See [CONTRIBUTING.md](CONTRIBUTING.md) or visit [start9.com/contribute](https://start9.com/contribute/).

To report security issues, email [security@start9.com](mailto:security@start9.com).
