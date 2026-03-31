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
- **i18n is mandatory** — any user-facing string must go in `core/locales/i18n.yaml` with all 5 locales (`en_US`, `de_DE`, `es_ES`, `fr_FR`, `pl_PL`). This includes CLI subcommand descriptions (`about.<name>`), CLI arg help (`help.arg.<name>`), error messages (`error.<name>`), notifications, setup messages, and any other text shown to users. Entries are alphabetically ordered within their section. See [i18n-patterns.md](i18n-patterns.md)
- When using DB watches, follow the `TypedDbWatch<T>` patterns in [patchdb.md](patchdb.md)
- **Always use `.invoke(ErrorKind::...)` instead of `.status()` when running CLI commands** via `tokio::process::Command`. The `Invoke` trait (from `crate::util::Invoke`) captures stdout/stderr and checks exit codes properly. Using `.status()` leaks stderr directly to system logs, creating noise. For check-then-act patterns (e.g. `iptables -C`), use `.invoke(...).await.is_ok()` / `.is_err()` instead of `.status().await.map_or(false, |s| s.success())`.
- Always use file utils in util::io instead of tokio::fs when available
