# AI Agent TODOs

Pending tasks for AI agents. Remove items when completed.

## Features

- [ ] Extract TS-exported types into a lightweight sub-crate for fast binding generation

  **Problem**: `make ts-bindings` compiles the entire `start-os` crate (with all dependencies: tokio,
  axum, openssl, etc.) just to run test functions that serialize type definitions to `.ts` files.
  Even in debug mode, this takes minutes. The generated output is pure type info — no runtime code
  is needed.

  **Goal**: Generate TS bindings in seconds by isolating exported types in a small crate with minimal
  dependencies.

  **Approach**: Create a `core/bindings-types/` sub-crate containing (or re-exporting) all 168
  `#[ts(export)]` types. This crate depends only on `serde`, `ts-rs`, `exver`, and other type-only
  crates — not on tokio, axum, openssl, etc. Then `build-ts.sh` runs `cargo test -p bindings-types`
  instead of `cargo test -p start-os`.

  **Challenge**: The exported types are scattered across `core/src/` and reference each other and
  other crate types. Extracting them requires either moving the type definitions into the sub-crate
  (and importing them back into `start-os`) or restructuring to share a common types crate.

- [ ] Use auto-generated RPC types in the frontend instead of manual duplicates

  **Problem**: The web frontend manually defines ~755 lines of API request/response types in
  `web/projects/ui/src/app/services/api/api.types.ts` that can drift from the actual Rust types.

  **Current state**: The Rust backend already has `#[ts(export)]` on RPC param types (e.g.
  `AddTunnelParams`, `SetWifiEnabledParams`, `LoginParams`), and they are generated into
  `core/bindings/`. However, commit `71b83245b` ("Chore/unexport api ts #2585", April 2024)
  deliberately stopped building them into the SDK and had the frontend maintain its own types.

  **Goal**: Reverse that decision — pipe the generated RPC types through the SDK into the frontend
  so `api.types.ts` can import them instead of duplicating them. This eliminates drift between
  backend and frontend API contracts.

- [ ] Auto-configure port forwards via UPnP/NAT-PMP/PCP - @dr-bonez

  **Goal**: When a binding is marked public, automatically configure port forwards on the user's router
  using UPnP, NAT-PMP, or PCP, instead of requiring manual router configuration. Fall back to
  displaying manual instructions (the port forward mapping from patch-db) when auto-configuration is
  unavailable or fails.

- [ ] Decouple createTask from service running state - @dr-bonez

  **Problem**: `createTask` currently depends on the service being in a running state.

  **Goal**: The `input-not-matches` handler in StartOS should queue the task, check it once the
  service is ready, then clear it if it matches. This allows tasks to be created regardless of
  whether the service is currently running.

- [ ] Implement URL plugins - @dr-bonez

  **Goal**: Add a plugin system that allows services to register URL scheme plugins, providing
  additional ways for other services to connect to them (e.g. alternative protocols or transports).

- [ ] OTA updates for start-tunnel - @dr-bonez

  **Goal**: Add an OTA update mechanism for the start-tunnel server so it can be updated in place
  without redeploying. start-tunnel is built from the same codebase as StartOS but runs as a
  separate service.
