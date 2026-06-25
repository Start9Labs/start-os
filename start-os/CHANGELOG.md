# Changelog

All notable changes to the StartOS OS product are documented here. The format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and StartOS
uses an [extended version](https://docs.start9.com) of semantic versioning.

Full per-release notes are published on the
[GitHub releases page](https://github.com/Start9Labs/start-os/releases). This
file tracks notable changes since the move to the monorepo.

## [Unreleased]

### Changed

- **Migrated `startos-backup-fs` into the monorepo** as the `start-os/backup-fs`
  workspace member (from the former `Start9Labs/start-fs` repo); it is no longer
  built as an external `cargo install --git` dependency.
- **Monorepo reorganization.** `start-os` is now the monorepo for all Start9
  products. The OS product moved into its own `start-os/` directory as a thin
  wrapper: the `startbox` and `start-container` entry points live in
  `src/bin/`, the admin UI and setup wizard in `web/`, and the container runtime
  in `container-runtime/`. Backend logic moved from the old `core/` crate to the
  shared `start-core` crate (`shared/crates/start-core`); shared Angular
  libraries moved to `shared/web`; the SDK to `start-sdk`; and the `patch-db`
  submodule to `vendor/patch-db`. Builds now run against the root Cargo and
  Angular workspaces (`cargo build -p start-os`, web from `shared/web`).

## [0.4.0-beta.10]

Current development version. See the
[release notes](https://github.com/Start9Labs/start-os/releases) when published.

## [0.4.0-beta.9] and earlier

See the [GitHub releases page](https://github.com/Start9Labs/start-os/releases)
for the full 0.4.0 beta and alpha history and all prior releases.

[Unreleased]: https://github.com/Start9Labs/start-os/compare/v0.4.0-beta.10...HEAD
[0.4.0-beta.10]: https://github.com/Start9Labs/start-os/compare/v0.4.0-beta.9...v0.4.0-beta.10
[0.4.0-beta.9]: https://github.com/Start9Labs/start-os/releases/tag/v0.4.0-beta.9
