# AGENTS.md

Agent/developer operating rules for the **start-os monorepo root**. This repo is the monorepo for all Start9 products. `CLAUDE.md` is a one-line `@AGENTS.md` import — do not edit it.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the layout and [CONTRIBUTING.md](CONTRIBUTING.md) for the build/test/format workflow.

## What lives where

Each product is a thin top-level wrapper; the bulk of the code lives in shared libs.

- `start-os/` — OS product. Rust bins `startbox` + `start-container` (`src/bin/*.rs`), `web/` (Angular UI + setup-wizard), `container-runtime/` (Node LXC service runtime), OS packaging (`debian/`, `apt/`, `assets/`, `build/`), `*.service`.
- `start-cli/` — `start-cli` bin (`src/main.rs`); thin wrapper over `start-core`.
- `start-registry/` — `registrybox` bin; registry server, serves the shared marketplace UI lib.
- `start-tunnel/` — `tunnelbox` bin + `web/` (StartTunnel UI).
- `start-sdk/` — `@start9labs/start-sdk` (`base/` + `package/`, kept cohesive) + `Makefile`/`s9pk.mk` + `docs/` (packaging mdbook).
- `brochure/` — marketing/landing Angular app.
- `shared/crates/start-core/` — the **entire** Rust backend lib (package `start-core`, lib name `startos`). All five bins depend on it. Internally unchanged from the old `core/` crate.
- `shared/web/` — single Angular workspace root (`angular.json`, `package.json`) + shared libs `shared/` (`@start9labs/shared`) and `marketplace/` (`@start9labs/marketplace`). Product apps reference these by package name.
- `vendor/patch-db/` — git submodule (Rust patch-db core + TS client), consumed by `start-core` and web.

## Operating rules

- **Polyglot repo.** Per-component gotchas live in component-level `AGENTS.md` files — read the relevant one before operating on that component (see Sub-scopes).
- **Verify cross-layer changes in order.** Rust → ts-bindings → SDK rebuild → web/container-runtime type checks. See [ARCHITECTURE.md](ARCHITECTURE.md#cross-layer-verification). Editing `start-sdk/base/lib/osBindings/*.ts` alone is NOT sufficient — the SDK bundle must be rebuilt before web/container-runtime see the change.
- **Ask before destructive `make` recipes.** Image flashing, deploy targets (`update*`, `reflash`, `wormhole*`), and `make clean*` consume hours and disk — confirm with the user first.
- **Use `make` recipes when they exist** rather than re-deriving the underlying commands. The root `Makefile` is a thin orchestrator that `include`s `build/common.mk` (shared vars/macros) and one `<project>/build.mk` per product — run everything from the repo root (`make all`, `make registry`, etc.); a product's targets live in its `build.mk`.
- **Build a single product** with `cargo build -p <crate> --bin <bin>` (bins: `startbox`/`start-container` in package `start-os`; `start-cli`; `registrybox` in `start-registry`; `tunnelbox` in `start-tunnel`).
- **Stale-path watch.** Old docs referenced `core/`, `web/`, `sdk/`, `container-runtime/`, `patch-db/` at the repo root. Those are gone — use the new locations above.

## Sub-scopes

- [`shared/crates/start-core/AGENTS.md`](shared/crates/start-core/AGENTS.md) — Rust backend
- [`shared/web/AGENTS.md`](shared/web/AGENTS.md) — Angular frontend workspace (UI, setup-wizard, brochure, shared libs)
- [`start-os/container-runtime/AGENTS.md`](start-os/container-runtime/AGENTS.md) — Node.js LXC service runtime
- [`start-sdk/AGENTS.md`](start-sdk/AGENTS.md) — TypeScript service-packaging SDK
- `vendor/patch-db/` — git submodule; edits belong upstream
