# Core — Rust Backend

The Rust backend daemon for StartOS.

## Binaries

- `startbox` — Main daemon (runs as `startd`)
- `start-cli` — CLI interface
- `start-container` — Runs inside LXC containers; communicates with host and manages subcontainers
- `registrybox` — Registry daemon
- `tunnelbox` — VPN/tunnel daemon

## Key Modules

- `src/context/` — Context types (RpcContext, CliContext, InitContext, DiagnosticContext)
- `src/service/` — Service lifecycle management with actor pattern (`service_actor.rs`)
- `src/db/model/` — Patch-DB models (`public.rs` synced to frontend, `private.rs` backend-only)
- `src/net/` — Networking (DNS, ACME, WiFi, Tor via Arti, WireGuard)
- `src/s9pk/` — S9PK package format (merkle archive)
- `src/registry/` — Package registry management

## RPC Pattern

See `rpc-toolkit.md` for JSON-RPC handler patterns and configuration.

## Patch-DB Patterns

Patch-DB provides diff-based state synchronization. Changes to `db/model/public.rs` automatically sync to the frontend.

**Key patterns:**
- `db.peek().await` — Get a read-only snapshot of the database state
- `db.mutate(|db| { ... }).await` — Apply mutations atomically, returns `MutateResult`
- `#[derive(HasModel)]` — Derive macro for types stored in the database, generates typed accessors

**Generated accessor types** (from `HasModel` derive):
- `as_field()` — Immutable reference: `&Model<T>`
- `as_field_mut()` — Mutable reference: `&mut Model<T>`
- `into_field()` — Owned value: `Model<T>`

**`Model<T>` APIs** (from `db/prelude.rs`):
- `.de()` — Deserialize to `T`
- `.ser(&value)` — Serialize from `T`
- `.mutate(|v| ...)` — Deserialize, mutate, reserialize
- For maps: `.keys()`, `.as_idx(&key)`, `.as_idx_mut(&key)`, `.insert()`, `.remove()`, `.contains_key()`

## i18n

See `i18n-patterns.md` for internationalization key conventions and the `t!()` macro.

## Rust Utilities & Patterns

See `core-rust-patterns.md` for common utilities (Invoke trait, Guard pattern, mount guards, Apply trait, etc.).
