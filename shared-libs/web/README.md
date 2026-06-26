# StartOS Web

The single [Angular](https://angular.dev/) + TypeScript workspace (Angular 22, [Taiga UI 5](https://taiga-ui.dev/)) shared by every Start9 front-end. This directory (`shared-libs/web`) is the workspace **root** — it holds `angular.json`, `package.json`, `tsconfig.json`, and the two shared libraries. The individual app projects live in their product directories and reference these libs.

## Libraries (in this directory)

- **`shared/`** — `@start9labs/shared`: API clients, common components, directives, pipes, services, types, and i18n shared by all apps.
- **`marketplace/`** — `@start9labs/marketplace`: service-discovery / marketplace UI, shared between the StartOS UI and the public marketplace.

## App projects (defined here, rooted elsewhere)

`angular.json` declares these applications; their `root`/`sourceRoot` point into the product directories:

- **ui** — primary StartOS admin interface — `../../start-os/web/ui`
- **setup-wizard** — initial-setup UI (`start.local`) — `../../start-os/web/setup-wizard`
- **start-tunnel** — StartTunnel VPN/forwarding management UI — `../../start-tunnel/web`
- **brochure** — public marketplace front (marketplace.start9.com); auto-deploys on merge to `master` — `../../brochure`

All four consume the `shared` and `marketplace` libs from this workspace.

## Quickstart

```sh
cd shared-libs/web
npm ci
npm run build:deps      # builds start-sdk + patch-db client (file: deps)
cp config-sample.json config.json
npm run start:ui        # mock-backed dev server
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for full setup, live-server proxying, and translation guides, and [ARCHITECTURE.md](ARCHITECTURE.md) for how the front end is structured.
