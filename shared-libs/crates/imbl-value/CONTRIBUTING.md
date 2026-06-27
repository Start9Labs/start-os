# Contributing to imbl-value

`imbl-value` is a first-party library crate at `shared-libs/crates/imbl-value` in the start-os monorepo.

## Documentation

Keep the doc set in sync with any change you make:

- **README.md** — what the crate is and how to use it.
- **ARCHITECTURE.md** — how it works: the `Value` enum, the modules, `InOMap`, and the optional features.
- **CONTRIBUTING.md** — this file: toolchain and workflow.
- **AGENTS.md** — rules and layout notes for AI agents. `CLAUDE.md` is a one-line `@AGENTS.md` import; don't
  put content in it — edit `AGENTS.md` instead.

If you change the public API, a module's role, or the build/feature story, update the matching doc in the
same PR.

## Prerequisites

- Rust stable for building and testing.
- Nightly `rustfmt` for formatting (the workspace formats with `cargo +nightly fmt`).
- No extra tooling is required; the `arbitrary` (proptest) and `ts-rs` features pull their deps in
  automatically when enabled.

## Building

From the repo root:

```bash
cargo build -p imbl-value
cargo build -p imbl-value --features arbitrary
cargo build -p imbl-value --features ts-rs
```

## Testing

From the repo root:

```bash
cargo test -p imbl-value
```

If you touch the proptest strategies, also run with `--features arbitrary`.

## Formatting

```bash
cargo +nightly fmt
```

Nightly is required — the workspace's rustfmt config relies on nightly-only options.

## Commits / PRs

- Use conventional-commit messages (`feat:`, `fix:`, `chore:`, `docs:` …).
- Keep PRs focused and small.
- Remember the downstream blast radius: `Value` is consumed by `patch-db`, `json-patch`, `json-ptr`,
  `jsonpath_lib`, `rpc-toolkit`, and `start_core`. A change to the public surface or to (de)serialization
  behavior can ripple into all of them — call it out in the PR and build the affected consumers if in doubt.
- Keep the docs above in sync with your change.
