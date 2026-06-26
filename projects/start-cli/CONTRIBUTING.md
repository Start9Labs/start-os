# Contributing to start-cli

`start-cli` is a thin `bin` crate over `start-core`. See the root
[CONTRIBUTING.md](../../CONTRIBUTING.md) for repo-wide workflow, commit, and review conventions;
this file covers what's specific to this crate.

## Where code lives

This crate holds only `src/main.rs` and `Cargo.toml`. **Almost all changes belong in
`start-core`** (`shared-libs/crates/start-core`):

- A new or changed CLI command → `main_api()` in
  `shared-libs/crates/start-core/src/lib.rs` and the relevant `src/<area>/` module.
- CLI argument/config changes → `start-core::context::config` (`ClientConfig`).
- The CLI entrypoint behavior → `shared-libs/crates/start-core/src/bins/start_cli.rs`.

Edit this crate only when the entrypoint, Cargo features, or bin wiring change.

## Build

From the monorepo root (single Cargo workspace, single `Cargo.lock`):

```sh
cargo build -p start-cli --bin start-cli
cargo build -p start-cli --bin start-cli --release
cargo check -p start-cli
```

## Test

No unit tests live in this crate. Exercise CLI behavior through `start-core` and by running
the built binary:

```sh
cargo test -p start-core                              # core tests (where CLI logic is tested)
cargo test -p start-core export_manpage_start_cli     # regenerate man pages
target/debug/start-cli --help                          # smoke test the bin
```

For end-to-end verification, build the binary and run it against a StartOS test VM (remote
commands need `-H https://<host>` + `auth login`; local commands like `s9pk`/`init-key`/`pubkey`
need no server).

## Format & lint

Use the repo-standard Rust tooling (run from the root, or scoped to this package):

```sh
cargo fmt
cargo clippy -p start-cli
```

## Pull requests

Follow the root contribution workflow: focused commits with conventional messages
(`fix:`, `feat:`, `chore:`), branch off the latest default branch, and open a PR. If your change
touches the CLI surface a user or package author sees, update the relevant docs in `start-sdk/docs`
(packaging) and the `start-os` docs in the same change set.
