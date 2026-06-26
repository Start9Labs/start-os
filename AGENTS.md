# AGENTS.md

Agent/developer operating rules for the **start-os monorepo root**. This repo is the monorepo for all Start9 products. `CLAUDE.md` is a one-line `@AGENTS.md` import — do not edit it.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the layout and [CONTRIBUTING.md](CONTRIBUTING.md) for the build/test/format workflow.

## What lives where

Each product lives under `projects/` as a thin wrapper; the bulk of the code lives in the top-level shared libs (`shared-libs/`).

- `projects/start-os/` — OS product. Rust bins `startbox` + `start-container` (`src/bin/*.rs`), `web/` (Angular UI + setup-wizard), `container-runtime/` (Node LXC service runtime), OS image build (`build/`), Debian control (`debian/`), VM-setup `assets/`, `backup-fs/`, `docs/`, `*.service`.
- `projects/start-cli/` — `start-cli` bin (`src/main.rs`); thin wrapper over `start-core`.
- `projects/start-registry/` — `registrybox` bin; registry server, serves the shared marketplace UI lib.
- `projects/start-tunnel/` — `tunnelbox` bin + `web/` (StartTunnel UI).
- `projects/start-sdk/` — `@start9labs/start-sdk` (`base/` + `package/`, kept cohesive) + `Makefile`/`s9pk.mk` + `docs/` (packaging mdbook).
- `projects/brochure-marketplace/` — public marketplace/landing Angular app (deploys to marketplace.start9.com).
- `projects/start-docs/` — the documentation website (build infra + landing + Bitcoin guides; each product's own book lives in its `docs/`).
- `shared-libs/crates/start-core/` — the **entire** Rust backend lib (package `start-core`, lib name `start_core`). All five bins depend on it. Internally unchanged from the old `core/` crate.
- `shared-libs/web/` — the shared Angular libs `shared/` (`@start9labs/shared`) and `marketplace/` (`@start9labs/marketplace`); the Angular workspace itself is rooted at the repo root (`angular.json`/`package.json`). Product apps reference the libs by package name.
- Top level also holds the shared build infra (`build/`, `Makefile`), `apt/`, the shared `debian/build.sh`, repo-level `scripts/`, `rfcs/` (protocol drafts), and `shared-libs/crates/patch-db/` (first-party crate, consumed by `start-core` and web).

## Operating rules

- **Polyglot repo.** Per-component gotchas live in component-level `AGENTS.md` files — read the relevant one before operating on that component (see Sub-scopes).
- **Verify cross-layer changes in order.** Rust → ts-bindings → SDK rebuild → web/container-runtime type checks. See [ARCHITECTURE.md](ARCHITECTURE.md#cross-layer-verification). Editing `start-sdk/base/lib/osBindings/*.ts` alone is NOT sufficient — the SDK bundle must be rebuilt before web/container-runtime see the change.
- **Ask before destructive `make` recipes.** Image flashing, deploy targets (`update*`, `reflash`, `wormhole*`), and `make clean*` consume hours and disk — confirm with the user first.
- **Use `make` recipes when they exist** rather than re-deriving the underlying commands. The root `Makefile` is a thin orchestrator that `include`s `build/common.mk` (shared vars/macros) and one `<project>/build.mk` per product (`projects/<name>/build.mk`, `shared-libs/*/build.mk`) — run everything from the repo root (`make all`, `make registry`, etc.); a product's targets live in its `build.mk`.
- **Build a single product** with `cargo build -p <crate> --bin <bin>` (bins: `startbox`/`start-container` in package `start-os`; `start-cli`; `registrybox` in `start-registry`; `tunnelbox` in `start-tunnel`).
- **Stale-path watch.** Old docs referenced `core/`, `web/`, `sdk/`, `container-runtime/`, `patch-db/` at the repo root, and the products + `shared/` directly at the root. Those are gone — products now live under `projects/`, the shared libs under `shared-libs/`; use the locations above.

## Sub-scopes

- [`projects/start-os/AGENTS.md`](projects/start-os/AGENTS.md) — OS product
- [`projects/start-os/container-runtime/AGENTS.md`](projects/start-os/container-runtime/AGENTS.md) — Node.js LXC service runtime
- [`projects/start-cli/AGENTS.md`](projects/start-cli/AGENTS.md) — CLI wrapper over `start-core`
- [`projects/start-registry/AGENTS.md`](projects/start-registry/AGENTS.md) — registry server wrapper
- [`projects/start-tunnel/AGENTS.md`](projects/start-tunnel/AGENTS.md) — tunnel server + UI
- [`projects/start-sdk/AGENTS.md`](projects/start-sdk/AGENTS.md) — TypeScript service-packaging SDK (packaging mdbook: [`docs/AGENTS.md`](projects/start-sdk/docs/AGENTS.md))
- [`projects/brochure-marketplace/AGENTS.md`](projects/brochure-marketplace/AGENTS.md) — public marketplace site
- [`projects/start-docs/AGENTS.md`](projects/start-docs/AGENTS.md) — documentation website
- [`shared-libs/AGENTS.md`](shared-libs/AGENTS.md) — shared libs container: [`crates/start-core`](shared-libs/crates/start-core/AGENTS.md) (Rust backend), [`web`](shared-libs/web/AGENTS.md) (Angular workspace + UI/setup-wizard/shared libs)
- `shared-libs/crates/patch-db/` — first-party crate (upstream: github.com/Start9Labs/patch-db)
