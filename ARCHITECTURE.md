# Architecture

This repo is the **monorepo for all Start9 products**. Each product is a thin top-level wrapper (its own bin entry points and product-specific frontend/packaging); the bulk of the code lives in shared libraries — `start-core` on the Rust side, a single Angular workspace plus the SDK on the web side.

## Tech Stack

- Backend: Rust (async/Tokio, Axum) — one library crate (`start-core`, lib name `startos`) shared by all bins
- Frontend: Angular 22 + TypeScript + Taiga UI 5 — one workspace rooted at `shared/web`
- Container runtime: Node.js/TypeScript with LXC
- Database/State: Patch-DB (git submodule at `vendor/patch-db`) — diff-based store with reactive frontend sync
- API: JSON-RPC via rpc-toolkit (see `shared/crates/start-core/rpc-toolkit.md`)
- Auth: password + session cookie, public/private key signatures, local authcookie (see `shared/crates/start-core/src/middleware/auth/`)

## Repository layout

```
start-os/                          # repo root (monorepo)
├── start-os/                      # OS product
│   ├── src/bin/{startbox,start-container}.rs
│   ├── web/                       #   Angular UI + setup-wizard
│   ├── container-runtime/         #   Node LXC service runtime
│   ├── debian/ apt/ assets/ build/
│   ├── *.service  services.slice
│   └── Cargo.toml                 #   → depends on start-core
├── start-cli/                     # start-cli bin (src/main.rs)
├── start-registry/                # registrybox bin; serves the shared marketplace lib
│   └── start-registryd.service
├── start-tunnel/                  # tunnelbox bin + web/ (StartTunnel UI)
│   └── start-tunneld.service
├── start-sdk/                     # @start9labs/start-sdk (base/ + package/) + Makefile/s9pk.mk + docs/
├── brochure/                      # marketing/landing Angular app
├── shared/
│   ├── crates/start-core/         # the ENTIRE startos lib (package start-core, lib name startos)
│   └── web/                       # Angular workspace root (angular.json, package.json)
│       ├── shared/                #   @start9labs/shared
│       └── marketplace/           #   @start9labs/marketplace
├── vendor/patch-db/               # git submodule (Rust core + TS client)
├── Cargo.toml  Cargo.lock         # one root Cargo workspace
└── Makefile                       # top-level build/test/deploy targets
```

## Components

- **`shared/crates/start-core/`** — the entire Rust backend, one library crate. Modules: `bins`, `registry`, `tunnel`, `service`, `s9pk`, `net`, `db`, `install`, `update`, `lxc`, `os_install`, `backup`, `sign`, `version`, … Handles RPC API, service lifecycle, networking (DNS, ACME, WiFi, Tor, WireGuard), backups, and database state. All product bins depend on it. See [shared/crates/start-core/ARCHITECTURE.md](shared/crates/start-core/ARCHITECTURE.md).

- **Product bins** — thin wrappers (feature toggles + `include_dir!` UI embed) over `start-core`:
  - `start-os` package → `startbox` (main daemon `startd`) and `start-container` (runs inside LXC containers)
  - `start-cli` → `start-cli`
  - `start-registry` → `registrybox` (package registry)
  - `start-tunnel` → `tunnelbox` (VPN/tunnel server)

- **`shared/web/`** — one Angular 22 / Taiga UI 5 workspace. `angular.json` defines six projects whose roots point into product dirs: `ui` and `setup-wizard` (`start-os/web/`), `start-tunnel` (`start-tunnel/web/`), `brochure` (`brochure/`), plus the two shared libraries `shared` (`@start9labs/shared`) and `marketplace` (`@start9labs/marketplace`). Apps talk to the backend exclusively via JSON-RPC. See [shared/web/ARCHITECTURE.md](shared/web/ARCHITECTURE.md).

- **`start-os/container-runtime/`** — Node.js runtime that runs inside each service's LXC container. Loads the service's JavaScript from its S9PK and manages subcontainers; talks to the host daemon via JSON-RPC over a Unix socket. See [start-os/container-runtime/AGENTS.md](start-os/container-runtime/AGENTS.md).

- **`start-sdk/`** — TypeScript SDK for packaging services (`@start9labs/start-sdk`). Kept cohesive: `base/` (core types, ABI, effects interface — consumed by web as `@start9labs/start-sdk-base`) and `package/` (full SDK for service developers — consumed by container-runtime as `@start9labs/start-sdk`). Its `Makefile`/`s9pk.mk` is the source of truth for the published tarball.

- **`vendor/patch-db/`** — git submodule providing diff-based state sync (CBOR encoded). Backend mutations produce diffs pushed to the frontend over WebSocket for reactive UI. See the [patch-db repo](https://github.com/Start9Labs/patch-db).

## Build pipeline

One root Cargo workspace (members: the product bin crates + `shared/crates/start-core`; `vendor/patch-db` excluded) and one Angular workspace at `shared/web`. Cross-layer changes flow in one direction:

```
Rust (shared/crates/start-core)
  → make ts-bindings: ts-rs export → shared/crates/start-core/bindings/ → rsync to start-sdk/base/lib/osBindings/
    → SDK build (cd start-sdk && make bundle) → baseDist/ + dist/
      → shared/web consumes baseDist/ (via @start9labs/start-sdk-base)
      → start-os/container-runtime consumes dist/ (via @start9labs/start-sdk)
```

Key make targets along the chain:

| Step | Command | What it does |
|---|---|---|
| 1 | `cargo check -p start-core` | Verify the backend lib compiles |
| 2 | `make ts-bindings` | Export ts-rs types → rsync to `start-sdk/base/lib/osBindings/` |
| 3 | `cd start-sdk && make bundle` | Build SDK `baseDist/` + `dist/` |
| 4 | `cd shared/web && npm run check` | Type-check Angular projects |
| 5 | `cd start-os/container-runtime && npm run check` | Type-check the runtime |

**Important**: editing `start-sdk/base/lib/osBindings/*.ts` alone is NOT enough — rebuild the SDK bundle (step 3) before web/container-runtime can see the change.

## Cross-layer verification

When a change spans Rust, SDK, web, and container-runtime, verify in the order above (1→5). `make ts-bindings` runs the `start-core` export and rsyncs `shared/crates/start-core/bindings/` → `start-sdk/base/lib/osBindings/`; the SDK bundle (step 3) is what web and container-runtime actually reference, not the source files.

## Data flow: backend → frontend

StartOS uses Patch-DB for reactive state sync:

1. The backend mutates state via `db.mutate()`, producing CBOR diffs.
2. Diffs are pushed to the frontend over a persistent WebSocket.
3. The frontend applies diffs to its local state copy and notifies observers.
4. Components watch specific DB paths via `PatchDB.watch$()`, receiving updates reactively.

The UI is therefore eventually consistent with the backend — after a mutating API call, the frontend waits for the corresponding PatchDB diff before resolving, so the UI reflects the result immediately.

## Further reading

- [shared/crates/start-core/ARCHITECTURE.md](shared/crates/start-core/ARCHITECTURE.md) — Rust backend
- [shared/web/ARCHITECTURE.md](shared/web/ARCHITECTURE.md) — Angular frontend
- [start-os/container-runtime/AGENTS.md](start-os/container-runtime/AGENTS.md) — container runtime
- [shared/crates/start-core/rpc-toolkit.md](shared/crates/start-core/rpc-toolkit.md) — JSON-RPC handler patterns
- [shared/crates/start-core/s9pk-structure.md](shared/crates/start-core/s9pk-structure.md) — S9PK package format
- [shared/crates/start-core/exver.md](shared/crates/start-core/exver.md) — extended versioning format
- [shared/crates/start-core/VERSION_BUMP.md](shared/crates/start-core/VERSION_BUMP.md) — version bumping guide
