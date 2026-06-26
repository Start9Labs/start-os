# Architecture

`brochure` is the public Start9 Marketplace web app. It is a small Angular 22 application that wraps the shared `@start9labs/marketplace` component library and feeds it data from one or more package registries.

## Place in the monorepo

This is one application project in the single Angular workspace rooted at `shared-libs/web`. It does not have its own `angular.json` or `package.json` — both live in `shared-libs/web`, and `brochure` is registered there as the `brochure` project (`shared-libs/web/angular.json`). Its `tsconfig.json` extends `shared-libs/web/tsconfig.json` and maps the shared libs:

- `@start9labs/marketplace` → `shared-libs/web/marketplace`
- `@start9labs/shared` → `shared-libs/web/shared`
- `@start9labs/start-sdk` → built from `start-sdk` (baseDist bundle)

Because brochure renders the same `@start9labs/marketplace` library used by the in-OS `ui` app, the marketplace experience is identical whether viewed here or inside a server.

## Source layout (`brochure/src`)

```
src/
  main.ts                       bootstraps AppComponent with APP_CONFIG
  index.html                    document shell + Open Graph/Twitter meta
  styles.scss                   app-level styles (Taiga theme comes from shared)
  globals.d.ts                  ambient types
  environments/
    environment.ts              dev: production:false  -> MockApiService
    environment.prod.ts         prod: production:true  -> LiveApiService
  app/
    app.component.ts            <tui-root tuiTheme="dark"> + <router-outlet>
    app.config.ts              ApplicationConfig: router, HttpClient, Taiga, i18n, DI bindings
    app.routes.ts              '' -> MainComponent (lazy), '**' -> redirect ''
    components/
      main.component.ts        the marketplace view (registry picker, tiles, CTA)
    services/
      api.service.ts           abstract ApiService (the data contract)
      live-api.service.ts      real impl: RPC over HTTP to registries (prod)
      mock-api.service.ts      fixture impl backed by api.fixures.ts (dev)
      marketplace.service.ts   AbstractMarketplaceService impl (registry state)
      marketplace.service spec / fixtures / icons / md-sample.md
```

## Data flow

1. `app.config.ts` binds `ApiService` to `LiveApiService` (prod) or `MockApiService` (dev) based on `environment.production`, and binds `AbstractMarketplaceService` to `MarketplaceService` via `tuiProvide`.
2. `MainComponent` injects `MarketplaceService` and renders the shared `<marketplace>` component, passing the current registry plus two-way-bound `category` and `query` signals. It adds a "Get a Start9 server" CTA and the registry picker.
3. `MarketplaceService` (extends `AbstractMarketplaceService` from the shared lib) owns registry selection state:
   - Default registries (Start9 + Community) come from `@start9labs/shared`'s `defaultRegistries`.
   - Custom registries are persisted in `localStorage` under the `_startos/` prefix and exposed reactively via a `BehaviorSubject`.
   - It fetches registry info + packages through `ApiService` and converts raw registry responses into `MarketplacePkg` objects; fetch failures emit on `registryError$`.
4. `LiveApiService` talks to registries with JSON-RPC over HTTP (`RELATIVE_URL` = `/rpc/v0`), using `@start9labs/shared`'s `HttpService`; it also proxies static package files (`LICENSE.md`, `instructions.md`). `MockApiService` returns the fixtures instead.

## UI stack

Taiga UI 5 + the shared marketplace/shared libraries. The app is dark-themed (`tuiTheme="dark"`, `provideTaiga({ mode: 'dark' })`). User-facing strings go through the `i18n` pipe (`I18N_PROVIDERS`), with dictionaries living in `shared-libs/web/shared/src/i18n`.

## Build & deploy

`@angular/build:application` builds to `brochure/dist/raw/brochure`. Assets are pulled from `shared/assets`, `brochure/src/assets/img`, and the Taiga icon set. Production builds apply the `environment.prod.ts` file replacement and output hashing. The deploy is automated on merge to `master` (`.github/workflows/deploy-brochure.yml`), which builds the SDK baseDist + patch-db client first, then the brochure bundle, and ships it to the VPS hosting marketplace.start9.com.
