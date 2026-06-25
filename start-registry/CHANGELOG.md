# Changelog

All notable changes to `start-registry` (the Start Registry server) are documented here. This project's version (`Cargo.toml`) tracks the StartOS release line. Format follows [Keep a Changelog](https://keepachangelog.com/).

## [Unreleased]

## [0.4.0-beta.10] — StartOS 0.4.0-beta.10

The registry server as it ships in this monorepo. Built as the multi-call binary `registrybox`, dispatching to `start-registryd` (server) and `start-registry` (CLI).

### Added

- Combined `index` endpoint exposing registry name, icon, package index, OS index, and signers.
- `info`, `os`, `package`, `admin`, `db`, and `metrics` RPC subcommands, all reachable from the `start-registry` CLI via `call-remote`.
- Package and OS asset hosting with Blake3 content commitments and signer verification.
- HTTP JSON-RPC (`/rpc`), WebSocket RPC continuations (`/ws/rpc`), and a REST continuation channel (`/rest/rpc`).
- Local + signature auth on the RPC route; admin-only commands gated by an `admin` metadata flag.
- Anonymous download/user metrics persisted in a SQLite database, surfaced via `metrics summary` / `metrics users` (admin-only).
- PatchDB-backed registry database (`<datadir>/registry.db`) with a migration runner.
- Configurable listen address (default `127.0.0.1:5959`), public hostname(s), Tor SOCKS proxy, data directory, and config-file path.
- `start-registryd.service` systemd unit and Debian packaging that installs `registrybox` as `/usr/bin/start-registrybox` with `start-registryd` / `start-registry` symlinks.

[Unreleased]: https://github.com/Start9Labs/start-os
[0.4.0-beta.10]: https://github.com/Start9Labs/start-os
