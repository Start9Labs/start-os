# AGENTS — StartOS OS product

Operating rules for AI developers working in `start-os/`. See the root
[AGENTS.md](../../AGENTS.md) for monorepo-wide rules and
[ARCHITECTURE.md](ARCHITECTURE.md) for how this product is wired.

## Where things are

- `src/bin/startbox.rs`, `src/bin/start-container.rs` — the only Rust in this
  dir. They are thin entry points; backend logic lives in
  `../../shared-libs/crates/start-core` (crate `start-core`, lib `start_core`).
- `web/ui`, `web/setup-wizard` — Angular apps; part of the workspace at
  `../../shared-libs/web`. Run web commands from `../../shared-libs/web`, not from here.
- `container-runtime/` — Node.js LXC runtime with its **own** AGENTS/CLAUDE;
  read `container-runtime/AGENTS.md` before touching it.
- `docs/` — the end-user mdbook (book "StartOS"), served at `/start-os/`.
- Systemd units + `services.slice` live directly in this dir; OS image
  packaging (`debian/`, `apt/`, `build/`, `assets/`) is at the **repo root**.

## Build & verify (run from the repo root)

- Compile the OS bins: `cargo check -p start-os` (or `cargo build -p start-os
  --bin startbox`). Local `cargo check` is **linux-only** — CI also builds
  apple-darwin and aarch64/riscv64 musl; platform-specific changes can pass here
  yet break those.
- Regenerate TS bindings after any change to exported Rust types:
  `make ts-bindings`. Then rebuild the SDK (`cd projects/start-sdk && make bundle`) before
  web/runtime type-checks — editing `start-sdk/base/lib/osBindings/*.ts` alone
  is not enough.
- Type-check web apps: `cd shared-libs/web && npm run check:ui && npm run check:setup`.
- Type-check the runtime: `cd projects/start-os/container-runtime && npm run check`.
- Build the UI: `make ui` (or `make uis` for ui + setup-wizard).
- Tests: `make test` (Rust + SDK + container-runtime), or `make test-core`.
- Format: `make format` / `make format-check`.

## Gotchas

- **UIs are embedded into `startbox` at compile time** (`include_dir!`), so the
  web build must precede the Rust build — use the `Makefile`, which encodes the
  ordering, rather than running `cargo build` against a stale `web/dist`.
- **`unshare-userns` must stay a multi-call applet**, not a CLI subcommand: it
  calls `unshare(CLONE_NEWUSER)`, which the kernel rejects on a multi-threaded
  process. See the comment in `src/bin/start-container.rs`.
- **Don't normalize style across components.** The container-runtime uses double
  quotes + no semicolons (its own prettier config); the SDK uses single quotes.
- **Don't edit `start-os/sdk.ts` / `index.ts`-style "DO NOT EDIT" files** or the
  root `s9pk.mk`.
- **Ask before destructive `make` recipes** — `update*`, `reflash`, `wormhole*`,
  image flashing, and `make clean*` consume hours/disk and may touch a live
  device.
- **The `beta` feature swaps the UI seed** (`patchdb-ui-seed.beta.json`) and
  forwards to `start-core`'s `beta` feature — keep both seeds in sync when you
  change seed shape.

## Docs are part of the change

User-facing changes (UI, CLI output/flags, install/setup flow) must update the
matching page under `docs/` in the same change. Keep this AGENTS, README, and
ARCHITECTURE current when you change structure, build steps, or conventions.
