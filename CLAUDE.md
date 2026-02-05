# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

StartOS is an open-source Linux distribution for running personal servers. It manages discovery, installation, network configuration, backups, and health monitoring of self-hosted services.

**Tech Stack:**
- Backend: Rust (async/Tokio, Axum web framework)
- Frontend: Angular 20 + TypeScript + TaigaUI
- Container runtime: Node.js/TypeScript with LXC
- Database/State: Patch-DB (git submodule) - storage layer with reactive frontend sync
- API: JSON-RPC via rpc-toolkit (see `agents/rpc-toolkit.md`)
- Auth: Password + session cookie, public/private key signatures, local authcookie (see `core/src/middleware/auth/`)

## Build Commands

```bash
# Set platform (x86_64, aarch64, riscv64, raspberrypi, x86_64-nonfree, aarch64-nonfree)
# Can export or set inline. Most recent platform is remembered if not specified.
export PLATFORM=x86_64
# or: PLATFORM=x86_64 make iso

# Development mode - sets dev environment, prevents rebuilds on git hash changes
. ./devmode.sh

# Full ISO build
make iso

# Faster development iterations (same network)
make update-startbox REMOTE=start9@<ip>  # Binary + UI only (fastest, no container runtime)
make update-deb REMOTE=start9@<ip>        # Full Debian package
make update REMOTE=start9@<ip>            # OTA-style update

# Remote device updates (different network, uses magic-wormhole)
make wormhole                              # Send startbox binary
make wormhole-deb                          # Send Debian package
make wormhole-squashfs                     # Send squashfs image

# Build specific components
make all                                   # All Rust binaries
make uis                                   # All web UIs
make ui                                    # Main UI only
make deb                                   # Debian package
```

## Testing

```bash
make test                    # All tests
make test-core               # Rust tests (via ./core/run-tests.sh)
make test-sdk                # SDK tests
make test-container-runtime  # Container runtime tests

# Run specific Rust test
cd core && cargo test <test_name> --features=test
```

## Code Formatting

```bash
make format              # Rust (requires nightly)
cd web && npm run check  # TypeScript type checking
cd web && npm run format # TypeScript/HTML/SCSS formatting (prettier)
```

## Architecture

### Core (`/core`)
The Rust backend daemon. Main binaries:
- `startbox` - Main daemon (runs as `startd`)
- `start-cli` - CLI interface
- `start-container` - Runs inside LXC containers; communicates with host and manages subcontainers
- `registrybox` - Registry daemon
- `tunnelbox` - VPN/tunnel daemon

**Key modules:**
- `src/context/` - Context types (RpcContext, CliContext, InitContext, DiagnosticContext)
- `src/service/` - Service lifecycle management with actor pattern (`service_actor.rs`)
- `src/db/model/` - Patch-DB models (`public.rs` synced to frontend, `private.rs` backend-only)
- `src/net/` - Networking (DNS, ACME, WiFi, Tor via Arti, WireGuard)
- `src/s9pk/` - S9PK package format (merkle archive)
- `src/registry/` - Package registry management

**RPC Pattern:** See `agents/rpc-toolkit.md`

### Web (`/web`)
Angular projects sharing common code:
- `projects/ui/` - Main admin interface
- `projects/setup-wizard/` - Initial setup
- `projects/start-tunnel/` - VPN management UI
- `projects/shared/` - Common library (API clients, components)
- `projects/marketplace/` - Service discovery

**Development:**
```bash
cd web
npm ci
npm run start:ui        # Dev server with mocks
npm run build:ui        # Production build
npm run check           # Type check all projects
```

### Container Runtime (`/container-runtime`)
Node.js runtime that manages service containers via RPC. See `RPCSpec.md` for protocol.

**Container Architecture:**
```
LXC Container (uniform base for all services)
└── systemd
    └── container-runtime.service
        └── Loads /usr/lib/startos/package/index.js (from s9pk javascript.squashfs)
            └── Package JS launches subcontainers (from images in s9pk)
```

The container runtime communicates with the host via JSON-RPC over Unix socket. Package JavaScript must export functions conforming to the `ABI` type defined in `sdk/base/lib/types.ts`.

**`/media/startos/` directory (mounted by host into container):**

| Path | Description |
|------|-------------|
| `volumes/<name>/` | Package data volumes (id-mapped, persistent) |
| `assets/` | Read-only assets from s9pk `assets.squashfs` |
| `images/<name>/` | Container images (squashfs, used for subcontainers) |
| `images/<name>.env` | Environment variables for image |
| `images/<name>.json` | Image metadata |
| `backup/` | Backup mount point (mounted during backup operations) |
| `rpc/service.sock` | RPC socket (container runtime listens here) |
| `rpc/host.sock` | Host RPC socket (for effects callbacks to host) |

**S9PK Structure:** See `agents/s9pk-structure.md` (TODO: create this doc)

### SDK (`/sdk`)
TypeScript SDK for packaging services (`@start9labs/start-sdk`).

- `base/` - Core types, ABI definitions, effects interface (`@start9labs/start-sdk-base`)
- `package/` - Full SDK for package developers, re-exports base

### Patch-DB (`/patch-db`)
Git submodule providing diff-based state synchronization. Changes to `db/model/public.rs` automatically sync to the frontend.

**Key patterns:**
- `db.peek().await` - Get a read-only snapshot of the database state
- `db.mutate(|db| { ... }).await` - Apply mutations atomically, returns `MutateResult`
- `#[derive(HasModel)]` - Derive macro for types stored in the database, generates typed accessors

**Generated accessor types** (from `HasModel` derive):
- `as_field()` - Immutable reference: `&Model<T>`
- `as_field_mut()` - Mutable reference: `&mut Model<T>`
- `into_field()` - Owned value: `Model<T>`

**`Model<T>` APIs** (from `db/prelude.rs`):
- `.de()` - Deserialize to `T`
- `.ser(&value)` - Serialize from `T`
- `.mutate(|v| ...)` - Deserialize, mutate, reserialize
- For maps: `.keys()`, `.as_idx(&key)`, `.as_idx_mut(&key)`, `.insert()`, `.remove()`, `.contains_key()`

## Environment Variables

- `PLATFORM` - Target platform
- `ENVIRONMENT` - Feature flags (comma-separated):
  - `dev` - Enables password SSH before setup, other developer conveniences
  - `unstable` - Adds debugging with performance penalty, enables throwing in non-prod scenarios
  - `console` - Enables tokio-console for async debugging
- `PROFILE` - Build profile (`release`, `dev`)
- `GIT_BRANCH_AS_HASH` - Use git branch as version hash

## TypeScript Bindings

Rust types export to TypeScript via ts-rs:
```bash
make ts-bindings  # Generates core/bindings/, copies to sdk/base/lib/osBindings/
```

## Multi-Platform Support

Platforms: `x86_64`, `x86_64-nonfree`, `aarch64`, `aarch64-nonfree`, `riscv64`, `raspberrypi`

The `-nonfree` variants include non-free firmware and drivers. Some platforms like `raspberrypi` also include non-free components by necessity.

The build system uses cross-compilation with zig for musl targets. Platform-specific code checks `PLATFORM` constant.

## Supplementary Documentation

The `agents/` directory contains detailed documentation and task tracking for AI assistants:

- `TODO.md` - Pending tasks for AI agents (check this first, remove items when completed)
- `rpc-toolkit.md` - JSON-RPC patterns and handler configuration
- `core-rust-patterns.md` - Common utilities and patterns for Rust code in `/core` (guard pattern, mount guards, etc.)
