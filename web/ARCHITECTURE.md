# Web Architecture

Angular 21 + TypeScript workspace using [Taiga UI 5](https://taiga-ui.dev/) component library.

## API Layer (JSON-RPC)

All backend communication uses JSON-RPC, not REST.

- **`HttpService`** (`shared/src/services/http.service.ts`) — Low-level HTTP wrapper. Sends JSON-RPC POST requests via `rpcRequest()`.
- **`ApiService`** (`ui/src/app/services/api/embassy-api.service.ts`) — Abstract class defining 100+ RPC methods. Two implementations:
  - `LiveApiService` — Production, calls the real backend
  - `MockApiService` — Development with mocks
- **`api.types.ts`** (`ui/src/app/services/api/api.types.ts`) — Namespace `RR` with all request/response type pairs.

**Calling an RPC endpoint from a component:**

```typescript
private readonly api = inject(ApiService)

async doSomething() {
  await this.api.someMethod({ param: value })
}
```

The live API handles `x-patch-sequence` headers — after a mutating call, it waits for the PatchDB WebSocket to catch up before resolving. This ensures the UI always reflects the result of the call.

## PatchDB (Reactive State)

The backend pushes state diffs to the frontend via WebSocket. This is the primary way components get data.

- **`PatchDbSource`** (`ui/src/app/services/patch-db/patch-db-source.ts`) — Establishes a WebSocket subscription when authenticated. Buffers updates every 250ms.
- **`DataModel`** (`ui/src/app/services/patch-db/data-model.ts`) — TypeScript type for the full database shape (`ui`, `serverInfo`, `packageData`).
- **`PatchDB<DataModel>`** — Injected service. Use `watch$()` to observe specific paths.

**Watching data in a component:**

```typescript
private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

// Watch a specific path — returns Observable, convert to Signal with toSignal()
readonly registry = toSignal(this.patch.watch$('ui', 'startosRegistry'))
readonly status = toSignal(this.patch.watch$('serverInfo', 'statusInfo'))
readonly packages = toSignal(this.patch.watch$('packageData'))
```

**In templates:** `{{ name() }}` — signals are called as functions.

## WebSockets

Three WebSocket use cases, all opened via `api.openWebsocket$<T>(guid)`:

1. **PatchDB** — Continuous state patches (managed by `PatchDbSource`)
2. **Logs** — Streamed via `followServerLogs` / `followPackageLogs`, buffered every 1s
3. **Metrics** — Real-time server metrics via `followServerMetrics`

## Navigation & Routing

- **Main app** (`ui/src/app/routing.module.ts`) — NgModule-based with guards (`AuthGuard`, `UnauthGuard`, `stateNot()`), lazy loading via `loadChildren`, `PreloadAllModules`.
- **Portal routes** (`ui/src/app/routes/portal/portal.routes.ts`) — Modern array-based routes with `loadChildren` and `loadComponent`.
- **Setup wizard** (`setup-wizard/src/app/app.routes.ts`) — Standalone `loadComponent()` per step.
- Route config uses `bindToComponentInputs: true` — route params bind directly to component `@Input()`.

## Forms

Two patterns:

1. **Dynamic (spec-driven)** — `FormService` (`ui/src/app/services/form.service.ts`) generates `FormGroup` from IST (Input Specification Type) schemas. Supports text, textarea, number, color, datetime, object, list, union, toggle, select, multiselect, file. Used for service configuration forms.

2. **Manual** — Standard Angular `FormGroup`/`FormControl` with validators. Used for login, setup wizard, system settings.

Form controls live in `ui/src/app/routes/portal/components/form/controls/` — each extends a base `Control<Spec, Value>` class and uses Taiga input components.

**Dialog-based forms** use `PolymorpheusComponent` + `TuiDialogContext` for modal rendering.

## i18n

- **`i18nPipe`** (`shared/src/i18n/i18n.pipe.ts`) — Translates English keys to the active language.
- **Dictionaries** live in `shared/src/i18n/dictionaries/` (en, es, de, fr, pl).
- Usage in templates: `{{ 'Some English Text' | i18n }}`

### How dictionaries work

- **`en.ts`** is the source of truth. Keys are English strings; values are numeric IDs (e.g. `'Domain Health': 748`).
- **Other language files** (`de.ts`, `es.ts`, `fr.ts`, `pl.ts`) use those same numeric IDs as keys, mapping to translated strings (e.g. `748: 'Santé du domaine'`).
- When adding a new i18n key:
  1. Add the English string and next available numeric ID to `en.ts`.
  2. Add the same numeric ID with a proper translation to every other language file.
  3. Always provide real translations, not empty strings.

## Services & State

Services often extend `Observable` and expose reactive streams via DI:

- **`ConnectionService`** — Combines network status + WebSocket readiness
- **`StateService`** — Polls server availability, manages app state (`running`, `initializing`, etc.)
- **`AuthService`** — Tracks `isVerified$`, triggers PatchDB start/stop
- **`PatchMonitorService`** — Starts/stops PatchDB based on auth state
- **`PatchDataService`** — Watches entire DB, updates localStorage bootstrap
