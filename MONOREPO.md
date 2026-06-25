# Monorepo reorganization proposal

Status: **implemented in this PR.** The directory reorganization, root Cargo
workspace, build-script/Makefile/CI rewiring, and TS path wiring are done.
Verified: `cargo check` of the whole workspace (all five bins) passes. The full
musl/docker image build and the cold Angular/SDK build chain are CI-grade and
are left for CI to validate (the wiring is updated but not run end-to-end here).

One deviation from the original plan below: the SDK is kept **cohesive** under
`start-sdk/` (both `base/` and `package/`) rather than splitting `base` into
`shared/web/sdk-base`. Reason: `start-sdk/package/lib` imports `base` via
relative paths (`../../base/lib/...`) under a shared tsc `rootDir`, and the
`dist` bundle places `base/lib` adjacent to the package — splitting them would
break every such import and violate `rootDir`. Extracting `sdk-base` cleanly is
a separate refactor (rewrite those imports to a package alias first).

## Goal

Make `start-os` the monorepo for all Start9 products. Each product gets a
**top-level folder** that is a thin wrapper — its own `main.rs` (and any
product-specific frontend/packaging). The bulk of the code lives in shared
libraries under `shared/`: `start-core` on the Rust side, a shared Angular
library + the SDK base on the web side.

Crucially, **`shared/crates/start-core` can stay a single crate** to begin
with. We do *not* have to untangle the internal module cycles
(`s9pk ↔ registry`, `net ↔ tunnel`) to get this layout — the product dirs hold
only the entry points, and `start-core` keeps everything else exactly as it is
today. Splitting `start-core` into finer crates later is optional cleanup, not a
prerequisite.

## Target layout

```
start-os/                          # repo root (monorepo)
├── start-os/                      # OS product
│   ├── src/bin/startbox.rs        #   was core/src/main/startbox.rs
│   ├── src/bin/start-container.rs #   was core/src/main/start-container.rs
│   ├── Cargo.toml                 #   → depends on start-core
│   ├── web/                       #   ui + setup-wizard (was web/projects/{ui,setup-wizard})
│   ├── container-runtime/         #   Node LXC service runtime (part of start-os)
│   ├── debian/ apt/ assets/ build/
│   ├── *.service  services.slice
│   └── Makefile
│
├── start-cli/
│   ├── src/main.rs                #   was core/src/main/start-cli.rs
│   └── Cargo.toml
│
├── start-registry/
│   ├── src/main.rs                #   was core/src/main/registrybox.rs
│   ├── start-registryd.service
│   └── Cargo.toml                 #   serves the shared marketplace lib; no own frontend
│
├── start-tunnel/
│   ├── src/main.rs                #   was core/src/main/tunnelbox.rs
│   ├── web/                       #   was web/projects/start-tunnel
│   ├── start-tunneld.service
│   └── Cargo.toml                 #   (absorbs the docs-only start-tunnel repo)
│
├── start-sdk/                     # @start9labs/start-sdk publish package
│   ├── Makefile  s9pk.mk          #   build wrapper; produces dist/ + baseDist/
│   ├── base/                      #   was sdk/base (@start9labs/start-sdk-base) — kept here
│   └── package/                   #   was sdk/package
│
├── brochure/                      # marketing site (was web/projects/brochure)
│
├── shared/
│   ├── crates/
│   │   └── start-core/            # the ENTIRE startos lib (package start-core,
│   │                              #   lib name `startos`), unchanged internally:
│   │                              #   src/{bins,registry,tunnel,service,s9pk,net,db,
│   │                              #   install,update,lxc,os_install,backup,sign,...}/
│   └── web/
│       ├── shared/                # shared Angular lib (was web/projects/shared)
│       └── marketplace/           # shared lib (was web/projects/marketplace)
│
└── vendor/
    └── patch-db/                  # relocated git submodule (still a submodule;
                                   #   not de-submoduled — see open question #2)
```

## Mapping from today

| Today | Moves to |
|---|---|
| `core/src/main/startbox.rs` | `start-os/src/bin/startbox.rs` |
| `core/src/main/start-container.rs` | `start-os/src/bin/start-container.rs` |
| `core/src/main/start-cli.rs` | `start-cli/src/main.rs` |
| `core/src/main/registrybox.rs` | `start-registry/src/main.rs` |
| `core/src/main/tunnelbox.rs` | `start-tunnel/src/main.rs` |
| `core/` (everything else: `src/lib.rs`, `src/bins/`, `src/registry/`, `src/tunnel/`, `src/service/`, …) | `shared/crates/start-core/` |
| `web/projects/ui`, `web/projects/setup-wizard` | `start-os/web/` |
| `web/projects/start-tunnel` | `start-tunnel/web/` |
| `web/projects/marketplace` | `shared/web/marketplace/` |
| `web/projects/shared` | `shared/web/shared/` |
| `web/projects/brochure` | `brochure/` |
| `container-runtime/` | `start-os/container-runtime/` |
| `sdk/base` | `start-sdk/base/` |
| `sdk/package` | `start-sdk/package/` |
| `patch-db` (submodule) | `vendor/patch-db/` (vendored) |

The `main.rs` entry points are already thin (~30 lines: feature toggles + the
`include_dir!` UI embed), so each product crate is genuinely a wrapper over
`start-core`.

## Build & tooling

- **One root Cargo workspace.** `Cargo.toml` at the repo root, members =
  the product bin crates (`start-os`, `start-cli`, `start-registry`,
  `start-tunnel`) + `shared/crates/start-core` + `vendor/patch-db/*`. Single
  `Cargo.lock`. Build a product with `cargo build -p <product>`.
- **One root npm workspace.** Workspace members under `shared/web/*` and each
  product's `web/` + `start-os/container-runtime/`. Consumers depend on the
  shared libs **by package name** instead of the current
  `file:../sdk/baseDist` / `file:../patch-db/client` artifact paths.
- **`include_dir!` web embedding.** `startbox`/`tunnelbox` embed the built UI at
  compile time. Since each app's web now lives inside its product dir, the embed
  paths were simply repointed to `$CARGO_MANIFEST_DIR/web/dist/static/...` in
  each bin (e.g. `start-os/web/dist/static/ui`), so no `build.rs` indirection is
  needed. The web-build-before-rust ordering stays encoded in the Makefile.
  Non-embedding bins (`start-cli`, `registrybox`, `start-container`) gain clean
  incremental rebuilds.
- **sqlx offline.** `sqlx(postgres)` is used in exactly one file
  (`version/v0_3_6_alpha_0.rs`, the legacy embassy DB migration). Run
  `cargo sqlx prepare` once and commit `shared/crates/start-core/.sqlx/` so the
  workspace builds with `SQLX_OFFLINE=true` and no live DB.
- **SDK publish.** `@start9labs/start-sdk` is a public dependency of every
  `*-startos` package, and its dual-artifact output (`baseDist` = base only,
  `dist` = base+package bundled) is not expressible by plain workspace hoisting.
  Keep its `Makefile`/`s9pk.mk` as the source of truth for the tarball and
  verify byte-compatible output before cutover.
- **CI / shared-workflows.** Repoint `cd core && cargo build` to workspace
  `-p`/`--bin` targets; preserve the existing
  x86_64/aarch64-apple-darwin + x86_64/aarch64/riscv64-linux-musl matrix.

## Migration path (history-preserving, each phase shippable)

- **P0 — vendor submodules.** `git subtree add` patch-db → `vendor/patch-db/`;
  drop the `.gitmodules` entry; repoint `core`'s patch-db path deps in-repo. No
  restructure.
- **P1 — introduce root workspaces.** Add the root Cargo workspace (`core`
  becomes a member as-is) and root npm workspace; reconcile the single
  `Cargo.lock` and the npm version skew the `file:` deps currently mask. Repoint
  CI. No moves yet.
- **P2 — `git mv` into shape.** Move the entry points into the product dirs,
  rename `core/` → `shared/crates/start-core/`, split `web/projects/*` into
  product `web/` dirs vs `shared/web/*`, move `container-runtime/` under
  `start-os/`, move `sdk/{base,package}`. Fix `file:`→name deps, Makefile paths,
  and add the `include_dir` `build.rs` indirection. No code-logic changes.

P0–P2 deliver the full folder-per-product monorepo with git history intact and
near-zero logic churn. Splitting `start-core` into finer crates is a later,
optional step.

## Open questions

1. **start-wrt** — fold in as another top-level product now (backend crates →
   `shared/crates`, web → its own dir; `openwrt` stays a submodule under
   `vendor/`), or keep separate for this pass? Note its Angular/Taiga version
   line differs from start-os's, so its `web` can't share `shared/web/shared`
   until those converge.
3. **patch-db release flow** — vendoring drops independent releases. Any external
   consumers of patch-db (Rust crates or the npm client) outside
   start-os/start-wrt that need it to stay separately publishable?
4. **Repo identity** — monorepo keeps living in the `start-os` repo, or a rename?
   Affects clone URLs, shared-workflows references, and the
   start-tunnel/start-wrt repo deprecation plan.
