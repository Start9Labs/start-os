# Web — Angular Frontend

Angular 20 + TypeScript workspace using [Taiga UI](https://taiga-ui.dev/) component library.

## Projects

- `projects/ui/` — Main admin interface
- `projects/setup-wizard/` — Initial setup
- `projects/start-tunnel/` — VPN management UI
- `projects/shared/` — Common library (API clients, components, i18n)
- `projects/marketplace/` — Service discovery

## Development

```bash
npm ci
npm run start:ui        # Dev server with mocks
npm run build:ui        # Production build
npm run check           # Type check all projects
```

## Golden Rules

1. **Taiga-first.** Use Taiga components, directives, and APIs whenever possible. Avoid hand-rolled HTML/CSS unless absolutely necessary. If Taiga has a component for it, use it.

2. **Pattern-match.** Nearly anything we build has a similar example elsewhere in this codebase. Search for existing patterns before writing new code. Copy the conventions used in neighboring components.

3. **When unsure about Taiga, ask or look it up.** Use `WebFetch` against `https://taiga-ui.dev/llms-full.txt` to search for component usage, or ask the user. Taiga docs are authoritative. See [Taiga UI Docs](#taiga-ui-docs) below.

## Taiga UI Docs

Taiga provides an LLM-friendly reference at `https://taiga-ui.dev/llms-full.txt` (~2200 lines covering all components with code examples). Use `WebFetch` to search it when you need to look up a component, directive, or API:

```
WebFetch url=https://taiga-ui.dev/llms-full.txt prompt="How to use TuiTextfield with a select dropdown"
```

When implementing something with Taiga, **also check existing code in this project** for local patterns and conventions — Taiga usage here may have project-specific wrappers or style choices.

## Architecture Overview

### API Layer (JSON-RPC)

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

### PatchDB (Reactive State)

The backend pushes state diffs to the frontend via WebSocket. This is the primary way components get data.

- **`PatchDbSource`** (`ui/src/app/services/patch-db/patch-db-source.ts`) — Establishes a WebSocket subscription when authenticated. Buffers updates every 250ms.
- **`DataModel`** (`ui/src/app/services/patch-db/data-model.ts`) — TypeScript type for the full database shape (`ui`, `serverInfo`, `packageData`).
- **`PatchDB<DataModel>`** — Injected service. Use `watch$()` to observe specific paths.

**Watching data in a component:**
```typescript
private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

// Watch a specific path — returns Observable, convert to Signal with toSignal()
readonly name = toSignal(this.patch.watch$('ui', 'name'))
readonly status = toSignal(this.patch.watch$('serverInfo', 'statusInfo'))
readonly packages = toSignal(this.patch.watch$('packageData'))
```

**In templates:** `{{ name() }}` — signals are called as functions.

### WebSockets

Three WebSocket use cases, all opened via `api.openWebsocket$<T>(guid)`:

1. **PatchDB** — Continuous state patches (managed by `PatchDbSource`)
2. **Logs** — Streamed via `followServerLogs` / `followPackageLogs`, buffered every 1s
3. **Metrics** — Real-time server metrics via `followServerMetrics`

### Navigation & Routing

- **Main app** (`ui/src/app/routing.module.ts`) — NgModule-based with guards (`AuthGuard`, `UnauthGuard`, `stateNot()`), lazy loading via `loadChildren`, `PreloadAllModules`.
- **Portal routes** (`ui/src/app/routes/portal/portal.routes.ts`) — Modern array-based routes with `loadChildren` and `loadComponent`.
- **Setup wizard** (`setup-wizard/src/app/app.routes.ts`) — Standalone `loadComponent()` per step.
- Route config uses `bindToComponentInputs: true` — route params bind directly to component `@Input()`.

### Forms

Two patterns:

1. **Dynamic (spec-driven)** — `FormService` (`ui/src/app/services/form.service.ts`) generates `FormGroup` from IST (Input Specification Type) schemas. Supports text, textarea, number, color, datetime, object, list, union, toggle, select, multiselect, file. Used for service configuration forms.

2. **Manual** — Standard Angular `FormGroup`/`FormControl` with validators. Used for login, setup wizard, system settings.

Form controls live in `ui/src/app/routes/portal/components/form/controls/` — each extends a base `Control<Spec, Value>` class and uses Taiga input components.

**Dialog-based forms** use `PolymorpheusComponent` + `TuiDialogContext` for modal rendering.

### i18n

- **`i18nPipe`** (`shared/src/i18n/i18n.pipe.ts`) — Translates English keys to the active language.
- **Dictionaries** live in `shared/src/i18n/dictionaries/` (en, es, de, fr, pl).
- Usage in templates: `{{ 'Some English Text' | i18n }}`

### Services & State

Services often extend `Observable` and expose reactive streams via DI:

- **`ConnectionService`** — Combines network status + WebSocket readiness
- **`StateService`** — Polls server availability, manages app state (`running`, `initializing`, etc.)
- **`AuthService`** — Tracks `isVerified$`, triggers PatchDB start/stop
- **`PatchMonitorService`** — Starts/stops PatchDB based on auth state
- **`PatchDataService`** — Watches entire DB, updates localStorage bootstrap

## Component Conventions

- **Standalone components** preferred (no NgModule). Use `imports` array in `@Component`.
- **`export default class`** for route components (enables direct `loadComponent` import).
- **`inject()`** function for DI (not constructor injection).
- **`signal()`** and `computed()`** for local reactive state.
- **`toSignal()`** to convert Observables (e.g., PatchDB watches) to signals.
- **`ChangeDetectionStrategy.OnPush`** on almost all components.
- **`takeUntilDestroyed(inject(DestroyRef))`** for subscription cleanup.

## Common Taiga Patterns

### Textfield + Select (dropdown)
```html
<tui-textfield tuiChevron>
  <label tuiLabel>Label</label>
  <input tuiSelect />
  <tui-data-list *tuiTextfieldDropdown>
    <button tuiOption [value]="item" *ngFor="let item of items">{{ item }}</button>
  </tui-data-list>
</tui-textfield>
```
Provider to remove the X clear button:
```typescript
providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })]
```

### Buttons
```html
<button tuiButton appearance="primary">Submit</button>
<button tuiButton appearance="secondary">Cancel</button>
<button tuiIconButton appearance="icon" iconStart="@tui.trash"></button>
```

### Dialogs
```typescript
// Confirmation
this.dialog.openConfirm({ label: 'Warning', data: { content: '...', yes: 'Confirm', no: 'Cancel' } })

// Custom component in dialog
this.dialog.openComponent(new PolymorpheusComponent(MyComponent, injector), { label: 'Title' })
```

### Toggle
```html
<input tuiSwitch type="checkbox" size="m" [showIcons]="false" [(ngModel)]="value" />
```

### Errors & Tooltips
```html
<tui-error [error]="[] | tuiFieldError | async" />
<tui-icon [tuiTooltip]="'Hint text'" />
```

### Layout
```html
<tui-elastic-container><!-- dynamic height --></tui-elastic-container>
<tui-scrollbar><!-- scrollable content --></tui-scrollbar>
<tui-loader [textContent]="'Loading...' | i18n" />
```
