# Contributing to yasi

`yasi` is a small first-party library at `shared-libs/crates/yasi`. For general environment setup,
cloning, and the monorepo build system, see the repo-root [CONTRIBUTING.md](../../../CONTRIBUTING.md).

## Documentation

This crate's docs split across four files:

- `README.md` — what this is and how to use it.
- `ARCHITECTURE.md` — how it works internally.
- `CONTRIBUTING.md` — this file; how to contribute.
- `AGENTS.md` — AI/dev operating rules (`CLAUDE.md` is a one-line `@AGENTS.md` import; don't edit it).

**Keep these in sync with the code.** If you change the API, the representations, the locking
discipline, the feature set, or the consumer list, update the relevant doc(s) in the same change.

## Prerequisites

- [Rust](https://rustup.rs) with a toolchain supporting **edition 2024** (`Cargo.toml` sets
  `edition = "2024"` and the code uses let-chains).
- Stable rustfmt is sufficient for this crate.

## Building

Run from the repo root:

```bash
cargo build -p yasi                  # default (no) features
cargo build -p yasi --all-features   # serde + ts-rs
```

## Testing

Run from the repo root:

```bash
cargo test -p yasi                   # unit tests, incl. the intern/drop deadlock regression
cargo test -p yasi --all-features    # also covers the serde and ts-rs modules
```

The `intern_drop_race_does_not_deadlock` test runs a multi-threaded stress loop for ~2 seconds with
a 30-second watchdog. If you touch the `TABLE` locking or `TableString::drop`, keep this test green —
it is the guard against the self-deadlock described in [ARCHITECTURE.md](ARCHITECTURE.md).

## Formatting

```bash
cargo fmt -p yasi
```

Stable rustfmt; no nightly required.

## Commits / PRs

- Conventional commits (`fix: …`, `feat: …`, `chore: …`).
- Keep PRs focused; update the docs above in the same change.
- Behavior changes that affect `exver` or `imbl-value` (the only consumers) should be considered
  against those crates' use before merging.
