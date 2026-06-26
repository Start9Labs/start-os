# Contributing to StartTunnel

StartTunnel is one product in the `start-os` monorepo. Start with the root
[`CONTRIBUTING.md`](../../CONTRIBUTING.md) for environment setup (Docker, Rust
toolchain, Node), collaboration channels, and repo-wide conventions. This file
covers what is specific to building, testing, and changing StartTunnel.

## Before you start

- Read [`AGENTS.md`](AGENTS.md) and [`ARCHITECTURE.md`](ARCHITECTURE.md).
- Most backend work happens in `shared-libs/crates/start-core/src/tunnel/`, **not** in
  this directory. The `start-tunnel/` dir is a thin wrapper (entry point, UI,
  systemd unit, docs).

## Build (from the repo root)

```bash
make tunnel                                   # full daemon build (UI + tunnelbox)
cargo build -p start-tunnel --bin tunnelbox   # cargo only (UI must be prebuilt)
cargo check -p start-tunnel                    # fast type-check
npm --prefix shared-libs/web run build:tunnel       # build just the Angular UI
make tunnel-deb                                # Debian package
```

`make tunnel` builds the UI, compresses it into
`web/dist/static/start-tunnel/`, then compiles `tunnelbox`, which embeds that UI
via `include_dir!`. Output:
`target/<arch>-unknown-linux-musl/<profile>/tunnelbox`.

## Test

```bash
make test-core    # backend tests — tunnel logic lives in start-core
```

Tunnel behavior is exercised by the `start-core` test suite (this crate is a
wrapper with no independent tests). For runtime verification, build the `.deb`
and install it on a Debian 13 VPS, or use a local VM. Backend changes should be
checked against the full CI matrix concerns — see the cross-platform note below.

## Format

```bash
make format          # apply formatting across the repo
make format-check    # verify (what CI runs)
```

Rust uses `cargo fmt` (edition 2024); web uses prettier via the shared Angular
config.

## Cross-platform

Local `cargo check`/`build` only covers Linux. CI also builds
`x86_64`/`aarch64-apple-darwin` and `riscv64`/`aarch64`/`x86_64` linux-musl. Any
change touching Rust dependencies or `libc`/platform APIs needs the darwin
target considered. For a code path that is dead on the other platform, `cfg`-gate
it rather than reimplementing cross-platform.

## Making a change

1. Branch off the latest default branch.
2. Make focused commits with conventional messages (`feat:`, `fix:`, `chore:`,
   `docs:`).
3. If you change the db schema, add a numbered migration in
   `shared-libs/crates/start-core/src/tunnel/migrations/` and register it in
   `migrations/mod.rs`.
4. If you change the API, regenerate TS bindings: `make ts-bindings`.
5. **Update docs in the same change** if the behavior is user-facing (UI, CLI
   flags/output, install flow, subnets/devices/forwarding). Docs live in
   `docs/src/` and publish to `start9.com/start-tunnel/`. Add a `CHANGELOG.md`
   entry under `## [Unreleased]`.
6. Run `make format-check` and `make test-core` before pushing.
7. Open a PR against the `start-os` repo.

## Versioning

The crate version lives in `Cargo.toml` (marked with `# VERSION_BUMP`). Record
notable changes in [`CHANGELOG.md`](CHANGELOG.md) following Keep a Changelog.
