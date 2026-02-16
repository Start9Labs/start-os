# Architecture

StartOS is an open-source Linux distribution for running personal servers. It manages discovery, installation, network configuration, backups, and health monitoring of self-hosted services.

## Tech Stack

- Backend: Rust (async/Tokio, Axum web framework)
- Frontend: Angular 20 + TypeScript + TaigaUI
- Container runtime: Node.js/TypeScript with LXC
- Database/State: Patch-DB (git submodule) - storage layer with reactive frontend sync
- API: JSON-RPC via rpc-toolkit (see `core/rpc-toolkit.md`)
- Auth: Password + session cookie, public/private key signatures, local authcookie (see `core/src/middleware/auth/`)

## Project Structure

```bash
/
├── assets/              # Screenshots for README
├── build/               # Auxiliary files and scripts for deployed images
├── container-runtime/   # Node.js program managing package containers
├── core/                # Rust backend: API, daemon (startd), CLI (start-cli)
├── debian/              # Debian package maintainer scripts
├── image-recipe/        # Scripts for building StartOS images
├── patch-db/            # (submodule) Diff-based data store for frontend sync
├── sdk/                 # TypeScript SDK for building StartOS packages
└── web/                 # Web UIs (Angular)
```

## Components

- **`core/`** — Rust backend daemon. Produces a single binary `startbox` that is symlinked as `startd` (main daemon), `start-cli` (CLI), `start-container` (runs inside LXC containers), `registrybox` (package registry), and `tunnelbox` (VPN/tunnel). Handles all backend logic: RPC API, service lifecycle, networking (DNS, ACME, WiFi, Tor, WireGuard), backups, and database state management. See [core/ARCHITECTURE.md](core/ARCHITECTURE.md).

- **`web/`** — Angular 20 + TypeScript workspace using Taiga UI. Contains three applications (admin UI, setup wizard, VPN management) and two shared libraries (common components/services, marketplace). Communicates with the backend exclusively via JSON-RPC. See [web/ARCHITECTURE.md](web/ARCHITECTURE.md).

- **`container-runtime/`** — Node.js runtime that runs inside each service's LXC container. Loads the service's JavaScript from its S9PK package and manages subcontainers. Communicates with the host daemon via JSON-RPC over Unix socket. See [container-runtime/CLAUDE.md](container-runtime/CLAUDE.md).

- **`sdk/`** — TypeScript SDK for packaging services for StartOS (`@start9labs/start-sdk`). Split into `base/` (core types, ABI definitions, effects interface, consumed by web as `@start9labs/start-sdk-base`) and `package/` (full SDK for service developers, consumed by container-runtime as `@start9labs/start-sdk`).

- **`patch-db/`** — Git submodule providing diff-based state synchronization. Uses CBOR encoding. Backend mutations produce diffs that are pushed to the frontend via WebSocket, enabling reactive UI updates without polling. See [patch-db repo](https://github.com/Start9Labs/patch-db).

## Build Pipeline

Components have a strict dependency chain. Changes flow in one direction:

```
Rust (core/)
  → cargo test exports ts-rs types to core/bindings/
    → rsync copies to sdk/base/lib/osBindings/
      → SDK build produces baseDist/ and dist/
        → web/ consumes baseDist/ (via @start9labs/start-sdk-base)
        → container-runtime/ consumes dist/ (via @start9labs/start-sdk)
```

Key make targets along this chain:

| Step | Command | What it does |
|---|---|---|
| 1 | `cargo check -p start-os` | Verify Rust compiles |
| 2 | `make ts-bindings` | Export ts-rs types → rsync to SDK |
| 3 | `cd sdk && make baseDist dist` | Build SDK packages |
| 4 | `cd web && npm run check` | Type-check Angular projects |
| 5 | `cd container-runtime && npm run check` | Type-check runtime |

**Important**: Editing `sdk/base/lib/osBindings/*.ts` alone is NOT sufficient — you must rebuild the SDK bundle (step 3) before web/container-runtime can see the changes.

## Cross-Layer Verification

When making changes across multiple layers (Rust, SDK, web, container-runtime), verify in this order:

1. **Rust**: `cargo check -p start-os` — verifies core compiles
2. **TS bindings**: `make ts-bindings` — regenerates TypeScript types from Rust `#[ts(export)]` structs
   - Runs `./core/build/build-ts.sh` to export ts-rs types to `core/bindings/`
   - Syncs `core/bindings/` → `sdk/base/lib/osBindings/` via rsync
   - If you manually edit files in `sdk/base/lib/osBindings/`, you must still rebuild the SDK (step 3)
3. **SDK bundle**: `cd sdk && make baseDist dist` — compiles SDK source into packages
   - `baseDist/` is consumed by `/web` (via `@start9labs/start-sdk-base`)
   - `dist/` is consumed by `/container-runtime` (via `@start9labs/start-sdk`)
   - Web and container-runtime reference the **built** SDK, not source files
4. **Web type check**: `cd web && npm run check` — type-checks all Angular projects
5. **Container runtime type check**: `cd container-runtime && npm run check` — type-checks the runtime

## Data Flow: Backend to Frontend

StartOS uses Patch-DB for reactive state synchronization:

1. The backend mutates state via `db.mutate()`, producing CBOR diffs
2. Diffs are pushed to the frontend over a persistent WebSocket connection
3. The frontend applies diffs to its local state copy and notifies observers
4. Components watch specific database paths via `PatchDB.watch$()`, receiving updates reactively

This means the UI is always eventually consistent with the backend — after any mutating API call, the frontend waits for the corresponding PatchDB diff before resolving, so the UI reflects the result immediately.

## Further Reading

- [core/ARCHITECTURE.md](core/ARCHITECTURE.md) — Rust backend architecture
- [web/ARCHITECTURE.md](web/ARCHITECTURE.md) — Angular frontend architecture
- [container-runtime/CLAUDE.md](container-runtime/CLAUDE.md) — Container runtime details
- [core/rpc-toolkit.md](core/rpc-toolkit.md) — JSON-RPC handler patterns
- [core/s9pk-structure.md](core/s9pk-structure.md) — S9PK package format
- [docs/exver.md](docs/exver.md) — Extended versioning format
- [docs/VERSION_BUMP.md](docs/VERSION_BUMP.md) — Version bumping guide
