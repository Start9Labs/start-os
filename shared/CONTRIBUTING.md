# Contributing to shared/

`shared/` groups two cross-product libraries. Contribute inside the relevant
sub-library; each has its own `CONTRIBUTING.md` with the full detail. Start from
the root [`CONTRIBUTING.md`](../CONTRIBUTING.md) for environment setup (Debian/
Ubuntu deps, Docker, Rust, Node) and the overall workflow.

## crates/start-core (Rust)

Part of the single root Cargo workspace.

```bash
# from the repo root
cargo build -p start-core
cargo check -p start-core
cargo fmt -p start-core                 # rustfmt config: shared/crates/start-core/rustfmt.toml
cd shared/crates/start-core && ./run-tests.sh
```

- Build by package name (`-p start-core`), never with a bare `cargo build` in the
  crate dir — there is one root `Cargo.toml` / `Cargo.lock`.
- Local `cargo check` is linux-only. CI builds an apple-darwin + linux-musl
  matrix; consider those targets for any change touching `libc`/platform APIs or
  dependencies (cfg-gate platform code rather than reimplementing it).
- See [`crates/start-core/CONTRIBUTING.md`](crates/start-core/CONTRIBUTING.md)
  and the topic notes (`core-rust-patterns.md`, `patchdb.md`, `rpc-toolkit.md`,
  `i18n-patterns.md`, `VERSION_BUMP.md`).

## web (Angular)

The single Angular workspace root for every front end in the monorepo.

```bash
cd shared/web
npm ci
npm run build:deps                      # build start-sdk bundle + patch-db client (required first)
npm run check                           # typecheck i18n, shared, marketplace, ui, setup, brochure
npm run format                          # prettier --write across all web projects
npm run format:check
```

- `build:deps` must run before any typecheck/build: `@start9labs/start-sdk`
  resolves to `../../start-sdk/baseDist` and `patch-db-client` to
  `../../vendor/patch-db/client`.
- Changes to `web/shared` or `web/marketplace` affect every app — run the full
  `npm run check` (it covers all projects) before opening a PR.
- Web UI work follows Taiga UI 5 conventions and mandatory i18n; see
  [`web/CONTRIBUTING.md`](web/CONTRIBUTING.md) and `web/AGENTS.md`.

## Conventions

- Conventional commit messages (`feat:`, `fix:`, `chore:`, `docs:`).
- Keep these docs current: if you change structure, build steps, or conventions,
  update the affected `README`/`ARCHITECTURE`/`AGENTS`/`CONTRIBUTING` in the same
  change. Do not edit `CLAUDE.md` (it is a one-line `@AGENTS.md` import).
- Format before committing (`cargo fmt -p start-core`, `npm run format`).
