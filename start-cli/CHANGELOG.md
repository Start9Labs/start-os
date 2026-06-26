# Changelog

All notable changes to `start-cli` are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the crate version tracks the
StartOS release version.

Because `start-cli` is a thin client over `start-core`, most user-visible CLI changes originate
in `start-core`; record here anything that changes this crate's entrypoint, features, packaging,
or the CLI's externally observable behavior.

## [Unreleased]

## [0.4.0-beta.10]

- Client for the StartOS RPC API, built as a thin bin over the shared `start-core` crate
  (package `start-core`, lib `start_core`).
- Remote command surface dispatched over HTTPS: `server`, `package`, `net`, `auth`, `db`,
  `ssh`, `wifi`, `disk`, `notification`, `backup`, `diagnostic`, `init`, `setup`, `kiosk`,
  `registry`, `tunnel`.
- Local developer tooling: `s9pk` packaging, `init-key`/`pubkey` developer keys, `util` helpers.
- `STARTOS_USE_PODMAN` toggles the local container backend for `s9pk` packaging (defaults to Docker).
- Deprecated `embassy-cli` alias retained for backward compatibility.

### Added

- **Packaging workspace commands (#3251).** `s9pk init-workspace [PATH]` provisions an AI-ready packaging workspace (shallow-clones start-docs, links `AGENTS.md`/`CLAUDE.md`, generates an ed25519 build key and a multi-profile `.startos/config.yaml`), and `init-package "<Name>"` scaffolds a package from the template. s9pk signing now uses the workspace build key, and `-H`/`-r` accept a profile name from the workspace config or a literal URL.

### Fixed

- The publish cookie store locks on a stable file to stop a publish race (#3291).
- `ws_continuation` honors `--root-ca` / `--insecure` (#3274).
- `choose` falls back to a generic non-tty prompt instead of failing when stdin isn't a terminal (#3265).

[Unreleased]: https://github.com/Start9Labs/start-os/compare/v0.4.0-beta.10...HEAD
[0.4.0-beta.10]: https://github.com/Start9Labs/start-os/releases/tag/v0.4.0-beta.10
