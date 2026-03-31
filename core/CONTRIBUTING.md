# Contributing to Core

For general environment setup, cloning, and build system, see the root [CONTRIBUTING.md](../CONTRIBUTING.md).

## Prerequisites

- [Rust](https://rustup.rs) (nightly for formatting)
- [rust-analyzer](https://rust-analyzer.github.io/) recommended
- [Docker](https://docs.docker.com/get-docker/) (for cross-compilation via `rust-zig-builder` container)

## Common Commands

```bash
cargo check -p start-os                    # Type check
cargo test --features=test                 # Run tests (or: make test-core)
make format                                # Format with nightly rustfmt
cd core && cargo test <test_name> --features=test  # Run a specific test
```

## Adding a New RPC Endpoint

1. Define a params struct with `#[derive(Deserialize, Serialize)]`
2. Choose a handler type (`from_fn_async` for most cases)
3. Write the handler function: `async fn my_handler(ctx: RpcContext, params: MyParams) -> Result<MyResponse, Error>`
4. Register it in the appropriate `ParentHandler` tree
5. If params/response should be available in TypeScript, add `#[derive(TS)]` and `#[ts(export)]`

See [rpc-toolkit.md](rpc-toolkit.md) for full handler patterns and all four handler types.

## Adding TS-Exported Types

When a Rust type needs to be available in TypeScript (for the web frontend or SDK):

1. Add `ts_rs::TS` to the derive list and `#[ts(export)]` to the struct/enum
2. Use `#[serde(rename_all = "camelCase")]` for JS-friendly field names
3. For types that don't implement TS (like `DateTime<Utc>`, `exver::Version`), use `#[ts(type = "string")]` overrides
4. For `u64` fields that should be JS `number` (not `bigint`), use `#[ts(type = "number")]`
5. Run `make ts-bindings` to regenerate — files appear in `core/bindings/` then sync to `sdk/base/lib/osBindings/`
6. Rebuild the SDK: `cd sdk && make baseDist dist`

## Adding i18n Keys

1. Add the key to `core/locales/i18n.yaml` with all 5 language translations
2. Use the `t!("your.key.name")` macro in Rust code
3. Follow existing namespace conventions — match the module path where the key is used
4. Use kebab-case for multi-word segments
5. Translations are validated at compile time

See [i18n-patterns.md](i18n-patterns.md) for full conventions.
