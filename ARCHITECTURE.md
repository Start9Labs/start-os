# Architecture

This repo is the **monorepo for all Start9 products**. Each product is a thin top-level wrapper (its own bin entry points and product-specific frontend/packaging); the bulk of the code lives in shared libraries — `start-core` on the Rust side, a single Angular workspace plus the SDK on the web side.

## Tech Stack

- Backend: Rust (async/Tokio, Axum) — one library crate (`start-core`, lib name `start_core`) shared by all bins
- Frontend: Angular 22 + TypeScript + Taiga UI 5 — one workspace rooted at the repo root (`angular.json`, `package.json` at root) with shared source libraries in `shared-libs/ts-modules`
- Container runtime: Node.js/TypeScript with LXC
- Database/State: Patch-DB (in-repo at `shared-libs/crates/patch-db`) — diff-based store with reactive frontend sync
- API: JSON-RPC via rpc-toolkit (see `shared-libs/crates/start-core/rpc-toolkit.md`)
- Auth: password + session cookie, public/private key signatures, local authcookie (see `shared-libs/crates/start-core/src/middleware/auth/`)

## Repository layout

```
start-os/                          # repo root (monorepo)
├── projects/start-os/             # OS product
│   ├── src/bin/{startbox,start-container}.rs
│   ├── web/                       #   Angular UI + setup-wizard
│   ├── container-runtime/         #   Node LXC service runtime
│   ├── debian/ apt/ assets/ build/
│   ├── *.service  services.slice
│   └── Cargo.toml                 #   → depends on start-core
├── projects/start-cli/            # start-cli bin (src/main.rs)
├── projects/start-registry/       # registrybox bin; serves the shared marketplace lib
│   └── start-registryd.service
├── projects/start-tunnel/         # tunnelbox bin + web/ (StartTunnel UI)
│   └── start-tunneld.service
├── projects/start-sdk/                     # @start9labs/start-sdk (base/ + package/) + Makefile/s9pk.mk + docs/
├── projects/brochure-marketplace/ # marketing/landing Angular app
├── shared-libs/
│   ├── crates/
│   │   ├── start-core/            # the ENTIRE backend lib (package start-core, lib name start_core)
│   │   ├── patch-db/              #   first-party crate (Rust core + TS client)
│   │   └── …                      #   exver, imbl-value, jsonpath, pi-beep, rpc-toolkit, yasi
│   └── ts-modules/                # Angular shared libs (workspace rooted at repo root)
│       ├── shared/                #   @start9labs/shared
│       └── marketplace/           #   @start9labs/marketplace
├── angular.json  package.json     # Angular workspace root
├── Cargo.toml  Cargo.lock         # one root Cargo workspace
└── Makefile                       # top-level build/test/deploy targets
```

## Components

- **`shared-libs/crates/start-core/`** — the entire Rust backend, one library crate. Modules: `bins`, `registry`, `tunnel`, `service`, `s9pk`, `net`, `db`, `install`, `update`, `lxc`, `os_install`, `backup`, `sign`, `version`, … Handles RPC API, service lifecycle, networking (DNS, ACME, WiFi, Tor, WireGuard), backups, and database state. All product bins depend on it. See [shared-libs/crates/start-core/ARCHITECTURE.md](shared-libs/crates/start-core/ARCHITECTURE.md).

- **Product bins** — thin wrappers (feature toggles + `include_dir!` UI embed) over `start-core`:
  - `start-os` package → `startbox` (main daemon `startd`) and `start-container` (runs inside LXC containers)
  - `start-cli` → `start-cli`
  - `start-registry` → `registrybox` (package registry)
  - `start-tunnel` → `tunnelbox` (VPN/tunnel server)

- **`shared-libs/ts-modules/`** — shared **TypeScript** modules consumed across products. The common thread is just that they are TS — the directory is not Angular-specific. Today it holds the two Angular 22 / Taiga UI 5 libraries `shared` (`@start9labs/shared`) and `marketplace` (`@start9labs/marketplace`). The single Angular workspace (root `angular.json`/`package.json`) defines six projects whose roots point into product dirs: `ui` and `setup-wizard` (`projects/start-os/web/`), `start-tunnel` (`projects/start-tunnel/web/`), `brochure-marketplace` (`projects/brochure-marketplace/`), plus those two libraries. Apps talk to the backend exclusively via JSON-RPC. See [shared-libs/ts-modules/ARCHITECTURE.md](shared-libs/ts-modules/ARCHITECTURE.md).

- **`projects/start-os/container-runtime/`** — Node.js runtime that runs inside each service's LXC container. Loads the service's JavaScript from its S9PK and manages subcontainers; talks to the host daemon via JSON-RPC over a Unix socket. See [projects/start-os/container-runtime/AGENTS.md](projects/start-os/container-runtime/AGENTS.md).

- **`projects/start-sdk/`** — TypeScript SDK for packaging services (`@start9labs/start-sdk`). Kept cohesive: `base/` (core types, ABI, effects interface — consumed by web as `@start9labs/start-sdk-base`) and `package/` (full SDK for service developers — consumed by container-runtime as `@start9labs/start-sdk`). Its `Makefile`/`s9pk.mk` is the source of truth for the published tarball.

- **`shared-libs/crates/patch-db/`** — first-party crate providing diff-based state sync (CBOR encoded). Backend mutations produce diffs pushed to the frontend over WebSocket for reactive UI. See the [patch-db repo](https://github.com/Start9Labs/patch-db).

## Build pipeline

One root Cargo workspace (members: the product bin crates + every shared crate under `shared-libs/crates/`, including `start-core` and the `patch-db` crates) and one Angular workspace rooted at the repo root (shared libs under `shared-libs/ts-modules`). Cross-layer changes flow in one direction:

```
Rust (shared-libs/crates/start-core)
  → make ts-bindings: ts-rs export → shared-libs/crates/start-core/bindings/ → rsync to projects/start-sdk/base/lib/osBindings/
    → SDK build (cd projects/start-sdk && make bundle) → baseDist/ + dist/
      → shared-libs/ts-modules consumes baseDist/ (via @start9labs/start-sdk-base)
      → projects/start-os/container-runtime consumes dist/ (via @start9labs/start-sdk)
```

Key make targets along the chain:

| Step | Command | What it does |
|---|---|---|
| 1 | `cargo check -p start-core` | Verify the backend lib compiles |
| 2 | `make ts-bindings` | Export ts-rs types → rsync to `projects/start-sdk/base/lib/osBindings/` |
| 3 | `cd projects/start-sdk && make bundle` | Build SDK `baseDist/` + `dist/` |
| 4 | `npm run check` | Type-check Angular projects (from the repo root) |
| 5 | `cd projects/start-os/container-runtime && npm run check` | Type-check the runtime |

**Important**: editing `projects/start-sdk/base/lib/osBindings/*.ts` alone is NOT enough — rebuild the SDK bundle (step 3) before web/container-runtime can see the change.

## Cross-layer verification

When a change spans Rust, SDK, web, and container-runtime, verify in the order above (1→5). `make ts-bindings` runs the `start-core` export and rsyncs `shared-libs/crates/start-core/bindings/` → `projects/start-sdk/base/lib/osBindings/`; the SDK bundle (step 3) is what web and container-runtime actually reference, not the source files.

## Data flow: backend → frontend

StartOS uses Patch-DB for reactive state sync:

1. The backend mutates state via `db.mutate()`, producing CBOR diffs.
2. Diffs are pushed to the frontend over a persistent WebSocket.
3. The frontend applies diffs to its local state copy and notifies observers.
4. Components watch specific DB paths via `PatchDB.watch$()`, receiving updates reactively.

The UI is therefore eventually consistent with the backend — after a mutating API call, the frontend waits for the corresponding PatchDB diff before resolving, so the UI reflects the result immediately.

## Further reading

- [shared-libs/crates/start-core/ARCHITECTURE.md](shared-libs/crates/start-core/ARCHITECTURE.md) — Rust backend
- [shared-libs/ts-modules/ARCHITECTURE.md](shared-libs/ts-modules/ARCHITECTURE.md) — Angular frontend
- [projects/start-os/container-runtime/AGENTS.md](projects/start-os/container-runtime/AGENTS.md) — container runtime
- [shared-libs/crates/start-core/rpc-toolkit.md](shared-libs/crates/start-core/rpc-toolkit.md) — JSON-RPC handler patterns
- [shared-libs/crates/start-core/s9pk-structure.md](shared-libs/crates/start-core/s9pk-structure.md) — S9PK package format
- [shared-libs/crates/start-core/exver.md](shared-libs/crates/start-core/exver.md) — extended versioning format
- [shared-libs/crates/start-core/VERSION_BUMP.md](shared-libs/crates/start-core/VERSION_BUMP.md) — version bumping guide
