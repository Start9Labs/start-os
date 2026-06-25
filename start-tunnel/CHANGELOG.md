# Changelog

All notable changes to StartTunnel are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- Relocated into the `start-os` monorepo as the `start-tunnel` product. The
  daemon and API now build from the shared `start-core` crate
  (`shared/crates/start-core/src/tunnel/`); the Angular UI is a project in the
  shared `shared/web` workspace.

## [0.4.0-beta.10]

Current version. WireGuard virtual private router with:

- Subnets — isolated private LANs over WireGuard.
- Devices — peers join a subnet and receive an IP and WireGuard config.
- Clearnet port forwarding — Layer 3/4 `iptables`/`nftables` DNAT, with SNI
  routing and upstream port mapping via IGD/UPnP and PCP.
- Web UI and `start-tunnel` CLI sharing a single JSON-RPC API.
- On-demand HTTPS with reactive cert handling driven by the database state.
