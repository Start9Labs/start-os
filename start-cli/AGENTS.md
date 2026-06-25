# AGENTS.md — start-cli

Agent/dev notes for the `start-cli` crate. Read the root
[`AGENTS.md`](../AGENTS.md) and [`CONTRIBUTING.md`](../CONTRIBUTING.md) first for repo-wide rules.

## What this is

A thin `bin` crate (`start-cli`) over `start-core` (cargo package `start-core`, lib `startos`,
at `shared/crates/start-core`). All CLI logic lives in `start-core`; this crate is just the
entrypoint (`src/main.rs`) and feature/bin wiring (`Cargo.toml`).

## Build & test

From the **monorepo root** (one Cargo workspace, one `Cargo.lock`):

```sh
cargo build -p start-cli --bin start-cli            # debug
cargo build -p start-cli --bin start-cli --release  # release
cargo check -p start-cli                             # fast type-check (linux-only locally)
target/debug/start-cli --help                        # smoke test
```

There are no tests in this crate itself. CLI behavior is tested in `start-core`; the man-page
generator is `cargo test -p start-core export_manpage_start_cli`.

## Gotchas

- **Don't add command logic here.** New/changed subcommands go in `start-core` —
  `main_api()` in `shared/crates/start-core/src/lib.rs` plus the relevant `src/<area>/` module.
  This crate changes only for entrypoint, feature, or bin-wiring edits.
- **`start-core` is referenced via the `startos` package alias** in `Cargo.toml`
  (`startos = { package = "start-core", path = "../shared/crates/start-core" }`). `src/main.rs`
  imports `startos::...`, not `start_core::...`.
- **Feature flags forward to `start-core`** (`beta`, `console`, `dev`, `test`, `unstable`);
  none on by default. Keep them in sync with `start-core`'s features when adding new ones.
- **`STARTOS_USE_PODMAN`** controls the local container backend for `s9pk` packaging. Unset or
  falsey → `PREFER_DOCKER` is set (Docker). Truthy (`1/true/y/yes`) → Podman.
- **In a StartOS image `start-cli` is a symlink to `startbox`** (see OS `Makefile`), the same
  `MultiExecutable` multiplexer. The standalone bin here enables only the `start-cli` sub-bin.
- **Stale paths:** older docs referenced `core/`, `web/`, `sdk/`, `patch-db/` at the repo root.
  They now live at `shared/crates/start-core`, `shared/web` + product `web/`, `start-sdk`,
  `vendor/patch-db`. Use the new paths.

## Verifying a command

Build the bin, then run it against a StartOS test VM (see root docs for `helix-vm` /
`start-cli` host flow). Remote commands need `-H https://<ip>` and an `auth login`; local
commands (`s9pk`, `init-key`, `pubkey`, `util`) need no server.
