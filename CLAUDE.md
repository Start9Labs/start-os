# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

StartOS is an open-source Linux distribution for running personal servers. It manages discovery, installation, network configuration, backups, and health monitoring of self-hosted services.

**Tech Stack:**
- Backend: Rust (async/Tokio, Axum web framework)
- Frontend: Angular 20 + TypeScript + TaigaUI
- Container runtime: Node.js/TypeScript with LXC
- Database/State: Patch-DB (git submodule) - storage layer with reactive frontend sync
- API: JSON-RPC via rpc-toolkit (see `core/rpc-toolkit.md`)
- Auth: Password + session cookie, public/private key signatures, local authcookie (see `core/src/middleware/auth/`)

## Build & Development

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Environment setup and requirements
- Build commands and make targets
- Testing and formatting commands
- Environment variables

**Quick reference:**
```bash
. ./devmode.sh                            # Enable dev mode
make update-startbox REMOTE=start9@<ip>   # Fastest iteration (binary + UI)
make test-core                            # Run Rust tests
```

### Verifying code changes

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

**Important**: Editing `sdk/base/lib/osBindings/*.ts` alone is NOT sufficient — you must rebuild the SDK bundle (step 3) before web/container-runtime can see the changes.

## Architecture

Each major component has its own `CLAUDE.md` with detailed guidance.

- **`core/`** — Rust backend daemon (startbox, start-cli, start-container, registrybox, tunnelbox)
- **`web/`** — Angular frontend workspace (admin UI, setup wizard, marketplace, shared library)
- **`container-runtime/`** — Node.js runtime managing service containers via JSON-RPC
- **`sdk/`** — TypeScript SDK for packaging services (`@start9labs/start-sdk`)
- **`patch-db/`** — Git submodule providing diff-based state synchronization

## Supplementary Documentation

The `docs/` directory contains cross-cutting documentation for AI assistants:

- `TODO.md` - Pending tasks for AI agents (check this first, remove items when completed)
- `USER.md` - Current user identifier (gitignored, see below)
- `exver.md` - Extended versioning format (used across core, sdk, and web)
- `VERSION_BUMP.md` - Guide for bumping the StartOS version across the codebase

Component-specific docs live alongside their code (e.g., `core/rpc-toolkit.md`, `core/i18n-patterns.md`).

### Session Startup

On startup:

1. **Check for `docs/USER.md`** - If it doesn't exist, prompt the user for their name/identifier and create it. This file is gitignored since it varies per developer.

2. **Check `docs/TODO.md` for relevant tasks** - Show TODOs that either:
   - Have no `@username` tag (relevant to everyone)
   - Are tagged with the current user's identifier

   Skip TODOs tagged with a different user.

3. **Ask "What would you like to do today?"** - Offer options for each relevant TODO item, plus "Something else" for other requests.
