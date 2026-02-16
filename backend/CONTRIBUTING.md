# Contributing — Backend

## Tech Stack

- **Rust** (2021 edition) with a 3-crate workspace
- **axum** + **tokio** for the HTTP server
- **[rpc-toolkit](https://github.com/Start9Labs/rpc-toolkit)** for JSON-RPC 2.0 (Start9Labs library)
- **uciedit** for parsing/writing OpenWrt UCI config files (workspace crate)
- **clap** for CLI argument parsing
- **serde** for serialization

## Getting Started

```bash
cargo build                          # Build all crates
cargo build -p startwrt-ctrl         # Build ctrl only
cargo build -p uciedit               # Build UCI library only
cargo test -p uciedit                # Run UCI parser tests
```

### Crates

| Crate | Description |
|-------|-------------|
| `ctrl` | RPC server (`startwrt-ctrld`) and CLI (`startwrt-cli`) |
| `uciedit` | UCI config parser/serializer with atomic writes and conflict detection |
| `uciedit_macros` | `#[derive(TypedSection)]` proc macro for typed UCI sections |

### Other Directories

- `firstboot_config/` — Template UCI configs applied on factory reset. Useful reference for expected config structure.
- `config_experiments/` — Historical UCI configs from manual testing on hardware.

## Architecture & Patterns

See [CLAUDE.md](CLAUDE.md) for detailed module documentation, UCI library internals, and backend conventions.
