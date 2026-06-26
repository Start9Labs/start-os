# Shared Libraries

Cross-product shared code for the Start9 monorepo. Everything here is consumed by
the thin product crates/apps at the repo root (`start-os/`, `start-cli/`,
`start-registry/`, `start-tunnel/`, `start-sdk/`, `brochure/`); little ships from
here on its own.

## Layout

- **`crates/start-core/`** — the entire Rust backend, packaged as the `start-core`
  crate (lib name `start_core`). Every product binary (`startbox`/`start-container`,
  `start-cli`, `registrybox`, `tunnelbox`) depends on it. See
  [`crates/start-core/README.md`](crates/start-core/README.md).

- **`web/`** — the single Angular workspace root (`angular.json`, `package.json`,
  `package-lock.json`) plus two publishable Angular libraries:
  - `web/shared/` → `@start9labs/shared` (API clients, components, i18n, styles)
  - `web/marketplace/` → `@start9labs/marketplace` (service discovery UI)

  The product apps (`start-os/web/ui`, `start-os/web/setup-wizard`,
  `start-tunnel/web`, `brochure`) all live in their product dirs but build through
  this workspace and import these libraries. See
  [`web/README.md`](web/README.md).

## Quickstart

Rust backend:

```bash
# from the repo root
cargo build -p start-core            # build the shared lib
cd shared-libs/crates/start-core && cargo test
```

Web:

```bash
cd shared-libs/web
npm ci
npm run build:deps                   # build start-sdk bundle + patch-db client
npm run check                        # typecheck all projects
```

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — how the two sub-libs fit into the monorepo
- [CONTRIBUTING.md](CONTRIBUTING.md) — build/test/format workflow
- [AGENTS.md](AGENTS.md) — practical notes for working in this directory
