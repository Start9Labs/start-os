# CLAUDE.md

Topical references: [rpc-toolkit.md](rpc-toolkit.md), [patchdb.md](patchdb.md), [i18n-patterns.md](i18n-patterns.md), [core-rust-patterns.md](core-rust-patterns.md), [s9pk-structure.md](s9pk-structure.md).

## Operating rules

- **Process invocation: use `.invoke(ErrorKind::...)`, not `.status()`.** When running CLI commands via `tokio::process::Command`, the `Invoke` trait (from `crate::util::Invoke`) captures stdout/stderr and checks exit codes. `.status()` leaks stderr directly to system logs and creates noise in production. For check-then-act patterns (e.g. `iptables -C`), use `.invoke(...).await.is_ok()` / `.is_err()` instead of `.status().await.map_or(false, |s| s.success())`.
- **File I/O: prefer `crate::util::io` over `tokio::fs`** when an equivalent helper exists. The repo's helpers add error context and mount-aware behavior that `tokio::fs` doesn't.
- **i18n is mandatory for any user-facing string** — including CLI subcommand descriptions (`about.<name>`), CLI arg help (`help.arg.<name>`), error messages, notifications, and setup messages. All 5 locales (`en_US`, `de_DE`, `es_ES`, `fr_FR`, `pl_PL`) must be filled in `core/locales/i18n.yaml`, alphabetically ordered within their section. See `i18n-patterns.md`. Compile-time validation will catch missing keys.
- **`#[ts(export)]` changes need a multi-step rebuild.** Editing a `#[ts(export)]` struct/enum in Rust does *not* update web/container-runtime — you must run `make ts-bindings` (regenerates `core/bindings/`, syncs to `sdk/base/lib/osBindings/`) and then `cd ../sdk && make baseDist dist` (rebuilds the SDK bundles that web/container-runtime actually consume). See [../ARCHITECTURE.md](../ARCHITECTURE.md#cross-layer-verification).
