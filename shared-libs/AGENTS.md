# AGENTS — shared/

This directory is a container for two cross-product libraries. There is no build
or test target for `shared/` itself — operate inside the relevant sub-library and
read its own `AGENTS.md` first.

## Where things are

- `crates/start-core/` — Rust backend lib (`start-core`, lib name `start_core`).
  Has its own `AGENTS.md`, `ARCHITECTURE.md`, `CONTRIBUTING.md`, plus topic notes
  (`core-rust-patterns.md`, `i18n-patterns.md`, `patchdb.md`, `rpc-toolkit.md`,
  `s9pk-structure.md`, `exver.md`, `VERSION_BUMP.md`).
- `web/` — Angular workspace root + the `@start9labs/shared` and
  `@start9labs/marketplace` libraries. Has its own `AGENTS.md` and
  `ARCHITECTURE.md` (Taiga UI 5 rules, component conventions, i18n).

## Build & test

Rust (`crates/start-core`) — runs from the root Cargo workspace:

```bash
cargo build -p start-core
cargo check -p start-core            # linux-only locally; CI also builds darwin + musl targets
cd shared/crates/start-core && ./run-tests.sh
cargo fmt -p start-core              # rustfmt config: crates/start-core/rustfmt.toml
```

Web (`web/`):

```bash
cd shared/web
npm ci
npm run build:deps                   # builds start-sdk bundle + patch-db client (required before typecheck/build)
npm run check                        # typechecks i18n, shared, marketplace, ui, setup, brochure
npm run format:check                 # prettier
```

## Gotchas

- **No code lives directly in `shared/`** — only the two sub-dirs. Don't add
  files at this level beyond these doc files.
- **start-core is one crate in one workspace.** Build it by package name
  (`-p start-core`), not by `cd`-ing and running a bare `cargo build`. There is a
  single root `Cargo.toml` / `Cargo.lock`.
- **Cross-platform matters for Rust.** Local `cargo check` is linux-only; CI
  builds an apple-darwin + linux-musl matrix. Changes touching `libc`/platform
  APIs or deps can break darwin even when linux passes — cfg-gate
  platform-specific code rather than reimplementing it cross-platform.
- **`web` is the workspace root for ALL front ends.** The product apps
  (`start-os/web/{ui,setup-wizard}`, `start-tunnel/web`, `brochure`) build through
  this workspace; their `angular.json` roots point into the product dirs. Editing
  a shared lib affects every app — run `npm run check` (all projects) after.
- **`build:deps` is a prerequisite.** `@start9labs/start-sdk` resolves to
  `../../start-sdk/baseDist` and `patch-db-client` to `../../vendor/patch-db/client`;
  both must be built before typecheck/build will succeed.
- **patch-db is a submodule** at repo-root `vendor/patch-db/` (not the old
  root `patch-db/`). start-core consumes its Rust `core`; web consumes its
  TS `client`.
- **Web UI work is Taiga UI 5 first** — see `web/AGENTS.md`; don't hand-roll
  HTML/CSS or guess Taiga APIs.
- Do not edit `CLAUDE.md` files — they are one-line `@AGENTS.md` imports.
