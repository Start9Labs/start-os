# Backend — StartWRT Rust Crates

Rust workspace: `ctrl` (RPC server + CLI), `uciedit` (UCI parser), `uciedit_macros` (proc macros). Single binary `startwrt` symlinked as `startwrt-ctrld` (daemon) and `startwrt-cli`.

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full backend architecture: transport, modules, UCI library, error types, common pitfalls.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and how-to guides.

## Quick Reference

```bash
cargo build                          # Build all crates
cargo build -p startwrt-ctrl         # Build ctrl only
cargo test -p uciedit                # Run UCI parser tests
```

## Operating Rules

- All handler modules export `pub fn <name><C: CtrlContext>() -> ParentHandler<C>` and are registered in `main_api()` in `ctrl/src/lib.rs`
- After UCI writes, call `/etc/init.d/<service> reload` — only when `ctx.effectful()` is true
- Use `uciedit`'s retry mechanism for writes that may conflict (profile creation retries 4 times)
- Use `STARTWRT_DEV_PASSWORD` env var for auth during development
- Generic `uci.get`/`uci.set`/`exec` are vestigial — all features have smart endpoints. See [../API_CONTRACT.md](../API_CONTRACT.md) for the full contract

## Key Files

| File | Purpose |
|------|---------|
| `ctrl/src/lib.rs` | `CtrlContext` trait, `main_api()`, `ServerContext`, `CliContext` |
| `ctrl/src/bins/daemon.rs` | Axum server setup (port 80/443), routes, TLS, setup mode |
| `ctrl/src/bins/cli.rs` | CLI dispatch, local-only subcommands (init, flash, verify) |
| `ctrl/src/middleware/auth.rs` | `SessionAuth` middleware (rate limiting, cookie validation) |
| `ctrl/src/profiles.rs` | Security profile CRUD (VLAN/firewall/DHCP orchestration) |
| `ctrl/src/error.rs` | `ErrorKind` enum, `Error` type |
| `uciedit/src/lib.rs` | `Config::parse()`, `dump()`, `parse_all()`, `dump_all()` |
| `uciedit/src/openwrt.rs` | All `TypedSection` structs (firewall, network, wireless, dhcp) |
