# brochure

The public **Start9 Marketplace** website — [marketplace.start9.com](https://marketplace.start9.com).

Despite the historical name "brochure," this is not a static landing page. It is a single-page Angular app that lets anyone browse the packages published to Start9's registries (and any arbitrary registry you point it at) without owning a server. It reuses the exact same `@start9labs/marketplace` UI that ships inside StartOS, so what you see here matches what a server owner sees in their marketplace tab.

The actual marketing/landing site lives elsewhere (`docs/landing`); this project is named for legacy reasons.

## Where it lives

This is a thin Angular application project inside the monorepo's single Angular workspace, rooted at `shared/web`:

- App source: `brochure/src/`
- Workspace + build config: `shared/web/angular.json` (project `brochure`), `shared/web/package.json`
- Shared libraries it consumes: `@start9labs/marketplace` (`shared/web/marketplace`) and `@start9labs/shared` (`shared/web/shared`)

Run all `npm` scripts from `shared/web`, not from this directory.

## Quickstart

```bash
cd shared/web
npm ci                     # install workspace deps (run once)
npm run build:deps         # build the SDK bundle + patch-db client (run once)

npm run start:brochure     # dev server on http://localhost:8200
```

The dev build uses `MockApiService` (fixtures in `src/app/services/api.fixures.ts`) so it runs with no live registry. Production builds swap in `LiveApiService`, which fetches real registries over RPC.

## Build

```bash
cd shared/web
npm run build:brochure     # production build -> brochure/dist/raw/brochure
```

## Deploy

Merges to `master` that touch `brochure/**` (or its shared deps) auto-deploy to marketplace.start9.com via `.github/workflows/deploy-brochure.yml`. There is no manual deploy step.

## More

- [ARCHITECTURE.md](ARCHITECTURE.md) — structure, services, data flow
- [CONTRIBUTING.md](CONTRIBUTING.md) — build/test/format workflow
- [AGENTS.md](AGENTS.md) — instructions for AI/dev agents working here
