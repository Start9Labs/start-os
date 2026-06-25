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
  (package `start-core`, lib `startos`).
- Remote command surface dispatched over HTTPS: `server`, `package`, `net`, `auth`, `db`,
  `ssh`, `wifi`, `disk`, `notification`, `backup`, `diagnostic`, `init`, `setup`, `kiosk`,
  `registry`, `tunnel`.
- Local developer tooling: `s9pk` packaging, `init-key`/`pubkey` developer keys, `util` helpers.
- `STARTOS_USE_PODMAN` toggles the local container backend for `s9pk` packaging (defaults to Docker).
- Deprecated `embassy-cli` alias retained for backward compatibility.

[Unreleased]: https://github.com/Start9Labs/start-os/compare/v0.4.0-beta.10...HEAD
[0.4.0-beta.10]: https://github.com/Start9Labs/start-os/releases/tag/v0.4.0-beta.10
