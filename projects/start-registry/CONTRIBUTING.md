# Contributing — start-registry

`start-registry` is a thin product wrapper in the `start-os` monorepo. The registry server/CLI implementation lives in the shared `start-core` crate (`shared-libs/crates/start-core/src/registry/` and `src/bins/registry.rs`), so most changes land there, not in this directory.

Start with the root [`CONTRIBUTING.md`](../../CONTRIBUTING.md) and [`AGENTS.md`](../../AGENTS.md) for repo-wide workflow, the doc map, and cross-layer verification rules. This file covers what's specific to the registry.

## Where to make changes

- **Server/CLI entry, RPC API, data model, persistence, migrations** → `shared-libs/crates/start-core/src/registry/` and `shared-libs/crates/start-core/src/bins/registry.rs`.
- **Bin wiring, systemd unit, docs** → this directory (`start-registry/`).
- **Browsing/search/download UI** → `shared-libs/ts-modules/marketplace/` (`@start9labs/marketplace`).

If you find yourself adding registry logic directly to `start-registry/src`, it almost certainly belongs in `start-core` instead.

## Build, test, format (from the monorepo root)

```bash
cargo build -p start-registry --bin registrybox   # build the wrapper bin
cargo check -p start-core                          # type-check the real logic
cargo test  -p start-core registry                 # registry tests
cargo clippy -p start-core                         # lints
cargo fmt                                           # format (run before committing)
make registry                                       # release musl build
```

Run a local server while iterating:

```bash
./target/debug/registrybox start-registryd --listen 127.0.0.1:5959 --datadir ./registry-data
./target/debug/registrybox start-registry --help
```

Run the checks that apply to what you touched and make sure `cargo fmt` is clean before opening a PR.

## Conventions

- **Rust 2024 edition**, formatted with `cargo fmt` (rustfmt). Keep clippy clean.
- **Comments:** default to none; clear names over prose. A comment is for a non-obvious *why* only — one short line.
- **API additions:** add subcommands in `registry/mod.rs`; use `with_call_remote::<CliContext>()` to expose them to the `start-registry` CLI and `with_about(...)` for help text. Tag admin-only commands with `with_metadata("admin", true)`.
- **Schema changes:** changing `RegistryDatabase` / index types requires a migration in `shared-libs/crates/start-core/src/registry/migrations`.
- **Version:** `Cargo.toml` `version` (with `# VERSION_BUMP`) tracks the OS release line — don't bump it independently.

## Docs are part of the change

Update the matching docs in the same PR:

- This dir's `README.md` / `ARCHITECTURE.md` for behavior, flags, or structure changes.
- `CHANGELOG.md` (Keep a Changelog style) for any user-visible change.
- OS-level packaging/registry docs in `start-os/docs`, and the marketplace UI, if you change the API contract or the install/run flow.

## Commits / PRs

Use Conventional Commit messages (`feat:`, `fix:`, `chore:`, `docs:`). Keep PRs focused; describe rationale in the PR body rather than in source comments.
