# Architecture — StartOS OS product

This document covers the **OS product** (`start-os/`). For the monorepo as a
whole, see [../ARCHITECTURE.md](../ARCHITECTURE.md) and [../MONOREPO.md](../MONOREPO.md).
For the Rust backend internals, see the `start-core` crate at
[../shared/crates/start-core](../shared/crates/start-core).

## What this product is

`start-os/` is a thin wrapper that assembles the StartOS server operating system
from shared building blocks. It contributes the entry points and the
OS-specific surface; almost all backend logic is in the `start-core` crate.

## Binaries

`Cargo.toml` declares package `start-os` with two binaries, both depending on
`start-core` (imported as the `startos` lib):

- **`startbox`** (`src/bin/startbox.rs`) — a multi-call applet enabling the
  `startd` daemon, the `start-cli` client, and the `unshare-userns` applet. It
  also embeds the built web UIs at compile time via `include_dir!`
  (`web/dist/static/ui`, `web/dist/static/setup-wizard`) and the Patch-DB UI
  seed (`web/patchdb-ui-seed.json`, or `*.beta.json` under the `beta` feature).
  Because the UIs are embedded, the web build must run before the Rust build —
  the `Makefile` encodes this ordering.
- **`start-container`** (`src/bin/start-container.rs`) — runs **inside** each
  service LXC. It enables `start-container` plus `unshare-userns`. The latter
  must be a multi-call applet (not a CLI subcommand): it calls
  `unshare(CLONE_NEWUSER)`, which the kernel rejects on a multi-threaded
  process, and the applet entry stays single-threaded.

Cargo features (`beta`, `console`, `dev`, `test`, `unstable`) forward to the
matching `start-core` features.

The sibling product crates (`start-cli`, `start-registry`, `start-tunnel`) are
analogous thin wrappers over the same `start-core` lib; the OS bins above are
this product's contribution.

## Web

Two Angular 22 apps live under `web/`:

- `ui/` — the admin dashboard (Angular project `ui`).
- `setup-wizard/` — the first-boot setup flow (project `setup-wizard`).

They are part of the single Angular workspace rooted at `../shared/web`
(`angular.json` there points each project's `root` at `../../start-os/web/...`).
They consume the shared `@start9labs/shared` and `@start9labs/marketplace` libs
from `../shared/web` and the SDK base from `../start-sdk`. The frontend talks to
the backend exclusively over JSON-RPC, with reactive state via Patch-DB.

`web/patchdb-ui-seed.json` / `patchdb-ui-seed.beta.json` seed initial UI state
and are embedded into `startbox`.

## Container runtime

`container-runtime/` is a Node.js program that runs inside every service LXC. It
loads the service's JavaScript from its `.s9pk`, manages subcontainers, and
talks to the host daemon over a Unix-socket JSON-RPC channel. It depends on the
**built** SDK at `../../start-sdk/dist`. It has its own
[README](container-runtime/README.md), [ARCHITECTURE](container-runtime/ARCHITECTURE.md),
and [AGENTS](container-runtime/AGENTS.md) — read those before editing it.

## Systemd units and cgroups

- `startd.service` — the main daemon (`Restart=always`, OOM-protected with
  `ManagedOOMPreference=avoid`).
- `services.slice` — the cgroup slice every service container lives under;
  `Delegate=yes` hands the subtree to LXC, and systemd-oomd kills the heaviest
  container under memory pressure rather than wedging the host. `startd` applies
  a RAM-dependent `MemoryMax`/`MemoryHigh` to this slice at boot.
- `startos-shutdown.service` — graceful teardown on power-off only (ties to
  `poweroff.target`/`halt.target`, not reboot); its `ExecStop` calls
  `start-cli server shutdown`.
- `startos-restart.service` — restart handling.

## OS image packaging

Image build inputs are shared at the repo root, not in this dir: `debian/`
(maintainer scripts incl. `debian/startos`), `apt/`, `build/` (`image-recipe`,
`firmware`, `env` scripts, migration images), and `assets/`. The root `Makefile`
turns the compiled bins + UIs + runtime squashfs into a `.deb`, `.squashfs`, and
finally the `.iso`/`.img` for the target platform (x86_64, aarch64/Raspberry Pi,
rockchip, riscv64).

## Build pipeline (cross-layer)

Changes flow in one direction; verify in this order:

```
start-core (Rust)
  → make ts-bindings   # cargo test exports ts-rs types → shared/crates/start-core/bindings/
    → start-sdk build  # produces start-sdk/baseDist (base) + start-sdk/dist (base+package)
      → web apps consume start-sdk/baseDist via @start9labs/start-sdk
      → container-runtime consumes start-sdk/dist
```

| Step | Command (from repo root) | What it does |
|---|---|---|
| 1 | `cargo check -p start-os` | Verify the OS bins compile |
| 2 | `make ts-bindings` | Export ts-rs types from `start-core` |
| 3 | `cd start-sdk && make bundle` | Build SDK `baseDist` + `dist` |
| 4 | `cd shared/web && npm run check:ui && npm run check:setup` | Type-check the apps |
| 5 | `cd start-os/container-runtime && npm run check` | Type-check the runtime |

Editing the generated bindings under `start-sdk/base/lib/osBindings/*.ts` alone
is **not** enough — the SDK bundle must be rebuilt before the web apps and
container-runtime see the change.

## Data flow: backend → frontend

StartOS uses Patch-DB (`../vendor/patch-db`) for reactive sync:

1. The backend mutates state via `db.mutate()`, producing CBOR diffs.
2. Diffs are pushed to the frontend over a persistent WebSocket.
3. The frontend applies the diff to its local copy and notifies observers.
4. Components watch specific DB paths and update reactively — no polling.

After a mutating RPC call, the frontend waits for the corresponding diff before
resolving, so the UI is always eventually consistent with the backend.

## Further reading

- [../shared/crates/start-core](../shared/crates/start-core) — Rust backend
- [../shared/web](../shared/web) — shared Angular libraries + workspace
- [container-runtime/ARCHITECTURE.md](container-runtime/ARCHITECTURE.md) — runtime
- [../MONOREPO.md](../MONOREPO.md) — monorepo layout and rationale
