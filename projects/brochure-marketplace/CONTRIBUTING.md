# Contributing to brochure

`brochure` is the public Start9 Marketplace web app (marketplace.start9.com). It is an application project inside the Angular workspace at `shared-libs/ts-modules`, so all tooling runs from there.

See the repo root [CONTRIBUTING.md](../../CONTRIBUTING.md) for org-wide conventions; this file covers the brochure-specific workflow.

## Setup

```bash
cd shared-libs/ts-modules
npm ci
npm run build:deps   # builds start-sdk baseDist + patch-db client — required before any brochure build
```

## Develop

```bash
cd shared-libs/ts-modules
npm run start:brochure   # dev server at http://localhost:8200 (MockApiService + fixtures)
```

The dev server uses mock data (`brochure/src/app/services/api.fixures.ts`). If your change affects the data contract (`ApiService`) or the shapes returned, update the fixtures so dev mode reflects reality. Production uses `LiveApiService` against real registries.

## Verify before pushing

From `shared-libs/ts-modules`:

```bash
npm run check:brochure   # type check (tsc --noEmit)
npm run build:brochure   # production build must succeed (this is what gets deployed)
npm run format:check     # prettier
```

Lint: `cd shared-libs/ts-modules && npm run ng -- lint brochure`.

Fix formatting with `npm run format` (formats all web projects, including brochure).

## Conventions

- **Reuse the shared libs.** Marketplace UI lives in `@start9labs/marketplace`; cross-app utilities/components in `@start9labs/shared`. Prefer them over new local code.
- **Follow workspace UI rules.** Taiga UI 5 for components/layout/styling; signals + `inject()` + OnPush; DI only on root (`app.config.ts`) or node injectors — never on routes. See `shared-libs/ts-modules/AGENTS.md` for the full list.
- **i18n every user-facing string** via the `i18n` pipe, with entries in every dictionary under `shared-libs/ts-modules/shared/src/i18n/dictionaries/`.
- **Keep changes scoped to `brochure/`.** Changes to shared libs, `angular.json`, or the SDK belong in those projects and affect every app.

## Deploy

Deployment is automatic: merging to `master` with changes under `brochure/**` (or its shared dependencies) triggers `.github/workflows/deploy-brochure.yml`, which builds and ships to marketplace.start9.com. Because there is no manual gate, make sure the production build passes locally before merging.
