# Core — Rust Backend

The Rust backend daemon for StartOS.

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for binaries, modules, Patch-DB patterns, and related documentation.

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to add RPC endpoints, TS-exported types, and i18n keys.

## Quick Reference

```bash
cargo check -p start-os                    # Type check
make test-core                             # Run tests
make ts-bindings                           # Regenerate TS types after changing #[ts(export)] structs
cd sdk && make baseDist dist               # Rebuild SDK after ts-bindings
```

## Operating Rules

- Always run `cargo check -p start-os` after modifying Rust code
- When adding RPC endpoints, follow the patterns in [rpc-toolkit.md](rpc-toolkit.md)
- When modifying `#[ts(export)]` types, regenerate bindings and rebuild the SDK (see [ARCHITECTURE.md](../ARCHITECTURE.md#build-pipeline))
- When adding i18n keys, add all 5 locales in `core/locales/i18n.yaml` (see [i18n-patterns.md](i18n-patterns.md))
