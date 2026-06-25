# Architecture

`start-cli` is intentionally tiny. Almost all behavior lives in the shared
[`start-core`](../shared/crates/start-core) crate (cargo package `start-core`, lib name
`startos`); this crate is just the `start-cli` entrypoint plus build/feature wiring.

## Place in the monorepo

```
start-os/                      monorepo root (one Cargo workspace, one Cargo.lock)
├── shared/crates/start-core/  the entire Rust backend (lib `startos`)
├── start-cli/        ← THIS CRATE — bin `start-cli`
├── start-os/         OS product (bins startbox, start-container)
├── start-registry/   registrybox bin
├── start-tunnel/     tunnelbox bin
└── start-sdk/        packaging SDK + docs
```

All product bins (`startbox`, `start-container`, `start-cli`, `registrybox`, `tunnelbox`)
depend on `start-core`. `start-cli` declares it as
`startos = { package = "start-core", path = "../shared/crates/start-core" }`.

## What this crate contains

- `Cargo.toml` — declares the `start-cli` bin, the `start-core` dependency, and the
  feature flags (`beta`, `console`, `dev`, `test`, `unstable`) that forward to `start-core`.
- `src/main.rs` — the entrypoint. It:
  1. Reads `STARTOS_USE_PODMAN`; if unset/false it sets `PREFER_DOCKER` (so local
     `s9pk` packaging uses Docker by default).
  2. Builds a `startos::bins::MultiExecutable`, enables the `start-cli` subcommand,
     sets it as the default, and `execute()`s.

`MultiExecutable` is the same multiplexer the OS uses for `startbox`: a single binary that
dispatches to a named sub-bin based on `argv[0]`/the first argument. `start-cli` enables only
the `start-cli` bin (and a deprecated `embassy-cli` alias) and defaults to it, so as a
standalone binary it behaves like a dedicated CLI.

## Request flow

```
argv ─▶ MultiExecutable ─▶ start_cli::main (in start-core)
                              │
                              ├─ build CliApp from start-core::main_api()
                              ├─ load ClientConfig (flags + .startos/config.yaml + /etc/startos)
                              └─ run:
                                   • remote command  ─▶ CliContext ─HTTPS RPC─▶ StartOS server
                                   • local command   ─▶ executed in-process (s9pk, keys, util)
```

The actual CLI bin (`start_cli::main`) lives in
[`shared/crates/start-core/src/bins/start_cli.rs`](../shared/crates/start-core/src/bins/start_cli.rs).
It constructs an `rpc_toolkit::CliApp` from `start-core::main_api()` and a `ClientConfig`,
renames the command to `start-cli`, stamps the current StartOS version, and runs it. Errors
are unwrapped from the RPC envelope and printed to stderr; the process exits with the RPC error code.

### Remote vs. local commands

`main_api()` (in [`shared/crates/start-core/src/lib.rs`](../shared/crates/start-core/src/lib.rs))
registers every subcommand once with `with_call_remote::<CliContext>()` — those are dispatched
over the network to a server's RPC endpoint, authenticated with the session cookie. Commands
without a remote handler (`s9pk`, `init-key`, `pubkey`, `util`) execute locally. The `registry`
and `tunnel` groups can target a separate registry/tunnel host via `--registry`/`--tunnel`.

### Configuration

`ClientConfig` (in `start-core::context::config`) is parsed from CLI flags and merged with
config files in priority order: explicit flags → workspace `.startos/config.yaml` →
`/etc/startos/config.yaml`. It carries the target host/registry/tunnel, proxy, cookie path,
developer key path, root CAs, and the `--insecure` toggle. The loaded config becomes a
`CliContext` that the RPC handlers use.

## Man pages

`start_cli.rs` has a `#[test] export_manpage_start_cli` that runs `clap_mangen` to write man
pages into `./man/start-cli`. Run it with `cargo test -p start-core export_manpage_start_cli`.

## Where to make changes

- New/changed CLI commands → `start-core` (`main_api()` and the relevant `src/<area>/` module),
  not here. This crate only changes when the entrypoint, features, or bin wiring change.
- Verifying a command end to end → build `start-cli` and run it against a test server/VM.
