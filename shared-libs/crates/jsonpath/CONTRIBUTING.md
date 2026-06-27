# Contributing to jsonpath

How to build, test, and contribute to `jsonpath_lib` (the crate at `shared-libs/crates/jsonpath`).
For general monorepo setup, see the repo-root [CONTRIBUTING.md](../../../CONTRIBUTING.md).

## Documentation

This sub-tree's docs split across four files:

- `README.md` — what this is and how to use it (the upstream usage/API reference).
- `ARCHITECTURE.md` — how it's built (parse → walk flow, public API surface, place in the monorepo).
- `CONTRIBUTING.md` — this file; how to contribute.
- `AGENTS.md` — AI/dev operating rules and layout map (`CLAUDE.md` is a one-line `@AGENTS.md`
  import; don't edit it).

**Keep these docs in sync with your changes.** When you change the module layout, public API,
build/test process, or fork-tracking status, update the relevant file(s) in the same change.

## Prerequisites

- [Rust](https://rustup.rs) (stable is sufficient to build and test). This is an **edition 2015**
  crate.
- Nightly toolchain only if you run the workspace formatter (`cargo +nightly fmt`).
- `imbl-value` is a sibling path dependency and builds automatically as part of the workspace.

## Building

Run from the repo root (the package name `jsonpath_lib` differs from the directory name):

```bash
cargo build -p jsonpath_lib          # rlib + cdylib
cargo build -p jsonpath_lib --lib    # rlib only (skip the cdylib)
```

## Testing

Run from the repo root:

```bash
cargo test -p jsonpath_lib                 # unit + integration + doc tests
cargo test -p jsonpath_lib --lib           # lib unit tests only
cargo test -p jsonpath_lib --test '*'      # integration tests only (tests/)
cargo test -p jsonpath_lib selector_delete # a single test by name
```

The `tests/` directory is a single integration-test crate with 11 modules; doc tests run from the
examples in `src/lib.rs` and `README.md`.

## Formatting

```bash
cargo +nightly fmt -p jsonpath_lib
```

The workspace uses nightly rustfmt. Note this crate carries pre-existing upstream warnings (unused
imports, lifetime elisions); don't churn unrelated lines to silence them.

## Commits / PRs

- Conventional commits (`fix:`, `feat:`, `chore:`, `docs:`).
- Keep PRs focused; update the docs above in the same change.
- **This is a vendored fork** of [freestrings/jsonpath](https://github.com/freestrings/jsonpath).
  Prefer pulling fixes from upstream over local divergence, and keep local changes minimal and
  well-described so they can be reconciled with upstream later. The main intentional divergence is
  operating on `imbl_value::Value` instead of `serde_json::Value`. New code should target the
  preferred `paths`/`selector` APIs, not the deprecated `parser`/`select`/`ffi` modules.
