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

- [ ] Make `SetupExecuteParams.password` optional in the backend - @dr-bonez

  **Problem**: In `core/src/setup.rs`, `SetupExecuteParams` has `password: EncryptedWire` (non-nullable),
  but the frontend needs to send `null` for restore/transfer flows where the user keeps their existing
  password. The `AttachParams` type correctly uses `Option<EncryptedWire>` for this purpose.

  **Fix**: Change `password: EncryptedWire` to `password: Option<EncryptedWire>` in `SetupExecuteParams`
  and handle the `None` case in the `execute` handler (similar to how `attach` handles it).

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
