# AGENTS.md — brochure

Practical instructions for agents working on the public Start9 Marketplace web app (marketplace.start9.com).

## What this is

A single-page Angular 22 app that wraps the shared `@start9labs/marketplace` library to browse package registries. It is **not** the marketing landing page (that's `docs/landing`), despite the "brochure" name. It is a **public website** that **auto-deploys on merge to `master`** via `.github/workflows/deploy-brochure.yml` — unlike `ui`, `setup-wizard`, and `start-tunnel`, which ship inside the OS image.

## Where things are

- App source: `brochure/src/` (this directory).
- Build/lint/serve config: `shared/web/angular.json` (project `brochure`) — **not** here.
- npm scripts: `shared/web/package.json`. Run them from `shared/web`.
- `brochure/tsconfig.json` extends `shared/web/tsconfig.json` and only adds path mappings; the real compiler settings live there.
- It consumes `@start9labs/marketplace` (`shared/web/marketplace`) and `@start9labs/shared` (`shared/web/shared`). Most UI building blocks already exist in those libs — reuse before reinventing.

## Build / run / check (always from `shared/web`)

```bash
cd shared/web
npm ci                 # once
npm run build:deps     # once: builds start-sdk baseDist + patch-db client (required)

npm run start:brochure # dev server, http://localhost:8200, uses MockApiService
npm run build:brochure # production build -> brochure/dist/raw/brochure
npm run check:brochure # tsc --noEmit type check for this project
npm run format         # prettier across web projects incl. brochure
```

Run `npm run check:brochure` and `npm run format:check` before pushing changes here.

## Gotchas

- **Dev vs prod data source.** `app.config.ts` picks the API impl from `environment.production`: dev → `MockApiService` (fixtures in `src/app/services/api.fixures.ts`), prod → `LiveApiService` (real RPC). When testing data-shape changes, update the fixtures or you'll see stale/empty results in dev.
- **`build:deps` is a prerequisite.** The app resolves `@start9labs/start-sdk` from `start-sdk/baseDist` and transitively needs the patch-db client. A fresh checkout that skips `build:deps` fails the brochure build/type-check.
- **Registry state lives in `MarketplaceService`** (`src/app/services/marketplace.service.ts`), which extends `AbstractMarketplaceService` from the shared lib. Custom registries persist in `localStorage` under the `_startos/` prefix. Don't add a parallel state store.
- **i18n is mandatory** for user-facing strings: route them through the `i18n` pipe and add entries to every dictionary under `shared/web/shared/src/i18n/dictionaries/`.
- **Taiga UI 5 does the UI.** Don't hand-roll HTML/CSS where Taiga has a primitive; verify Taiga APIs against the docs, not memory. See `shared/web/AGENTS.md` for the workspace-wide Taiga idioms (DI on root/node only, `tuiProvide`, attribute-selector controls, etc.).
- **Public deploy.** A merge to `master` touching `brochure/**` (or its shared deps) ships to production immediately. Test the production build locally before merging.

## Scope

Edit only `brochure/`. Workspace-wide concerns (shared libs, angular.json, SDK) belong to their own projects — coordinate there. Don't edit `CLAUDE.md` (it's just `@AGENTS.md`).
