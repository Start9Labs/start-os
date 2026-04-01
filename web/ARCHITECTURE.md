# Frontend Architecture

Angular 21 single-page application for the StartWRT admin UI. TypeScript 5.9, Taiga UI v5, zoneless change detection, signal-based state, standalone components. No NgModules.

## Project Structure

```
src/app/
├── app.ts                    # Root component (CSS Grid layout, TuiRoot host)
├── app.config.ts             # Providers: Taiga, routing, API, HTTP
├── app.routes.ts             # Top-level routes (auth guard splits login/setup/home)
│
├── components/               # Shared UI components
│   ├── aside.ts              # Right help panel (route-driven content)
│   ├── ca-wizard.ts          # CA certificate install wizard
│   ├── copy.ts               # Copy-to-clipboard directive
│   ├── footer.ts             # Form footer (Cancel / Save buttons)
│   ├── form.ts               # [formLoading] directive — TuiForm + TuiCardLarge + TuiSkeleton
│   ├── header.ts             # Top bar: search, help toggle, system info
│   ├── masked.ts             # Mask sensitive text with toggle + copy
│   ├── nav.ts                # Left sidebar navigation
│   ├── placeholder.ts        # Empty-state placeholder with icon
│   ├── reconnecting-dialog.ts# Reconnecting overlay during network restarts
│   ├── summary.ts            # Read-only display with copy button
│   └── timeline.ts           # Timeline/activity display
│
├── help/                     # Help panel directives
│   ├── help.ts               # *help — projects content into the aside panel
│   └── modal-help.ts         # *modalHelp — same but for dialogs
│
├── pipes/
│   ├── markdown.pipe.ts      # {{ value | markdown }} via `marked`
│   └── to-camel.pipe.ts      # {{ value | toCamel }}
│
├── services/
│   ├── action.service.ts     # Wraps async ops with loading/success/error toasts
│   ├── auth.service.ts       # Signal-based auth state + localStorage
│   ├── form.service.ts       # Abstract FormService<T> — load/save/refresh cycle
│   ├── http.service.ts       # Low-level HttpClient.post wrapper
│   ├── network-restart.service.ts # Network error suppression during restarts
│   ├── rpc.service.ts        # JSON-RPC 2.0 over HTTP (auto-logout on code 34)
│   ├── sidebar.service.ts    # Sidebar visibility signals
│   ├── system.service.ts     # System info, version, update check
│   └── api/
│       ├── api.service.ts    # Abstract API contract (all RPC methods)
│       ├── live-api.service.ts
│       ├── mock-api.service.ts
│       └── types.ts          # UCI section types + API response types
│
├── utils/
│   ├── validators.ts         # Custom validators: ipv4, ipv6, prefix, mac, hostname, etc.
│   ├── pauseFor.ts           # Promise-based delay
│   ├── languages.ts          # Supported locales
│   ├── timezones.ts          # Timezone list
│   ├── schedule.ts           # Schedule window utilities
│   ├── masks.ts              # Input masks for forms
│   └── workspace-config.ts   # Type for config.json
│
├── login.ts                  # Login page
│
└── routes/
    ├── setup.ts              # Initial password setup
    ├── setup-wizard/         # First-time device setup (flash from microSD)
    ├── wan/                  # WAN config (ipv4, ipv6, mac, dns, ddns)
    ├── published-ports/      # Port forwarding — BEST example of table + dialog pattern
    ├── outbound/             # Outbound VPN (WireGuard clients)
    ├── inbound/              # Inbound VPN (WireGuard server + peers)
    ├── ethernet/             # Ethernet port config
    ├── wifi/                 # WiFi (passwords, blackout schedule, settings)
    ├── lan/                  # LAN settings (ipv4, ipv6)
    ├── devices/              # Device management
    ├── profiles/             # Security profiles
    └── settings/             # General, advanced, password, SSH keys, logs, activity, backup
```

## Routing

Four auth-based guards determine which view loads:

1. **Setup wizard** — `AuthService.setupMode()` is true (booted from removable media)
2. **Initial password** — `AuthService.initialized()` is false (no admin password set)
3. **Authenticated** — `AuthService.authenticated()` is true → full app with nav/aside
4. **Login** — fallback when not authenticated

Authenticated routes are children of the root `App` component (CSS Grid with header, nav, main, aside). All routes are lazy-loaded via `loadComponent` / `loadChildren`.

## Route Anatomy

Each route folder typically contains:

| File         | Purpose                                      |
| ------------ | -------------------------------------------- |
| `index.ts`   | Page component (default export, lazy-loaded) |
| `service.ts` | Extends `FormService<T>` — load/save data    |
| `table.ts`   | Table component (for list views)             |
| `dialog.ts`  | Dialog component for add/edit flows          |
| `types.ts`   | Local types and interfaces                   |

Multi-level routes (wan, wifi, settings, profiles) use tab layouts with child routes:

```typescript
export default [
  {
    path: '',
    component: TabsComponent,
    children: [
      { path: 'ipv4', loadComponent: () => import('./routes/ipv4') },
      { path: 'ipv6', loadComponent: () => import('./routes/ipv6') },
      { path: '**', redirectTo: 'ipv4' },
    ],
  },
] satisfies Routes
```

## Key Patterns

### Page Component (form-based)

```typescript
@Component({
  template: `
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form
      [formGroup]="form"
      [formLoading]="!service.data()"
      (reset.prevent)="form.reset(service.data())"
      (ngSubmit)="onSave()"
    >
      <!-- form fields -->
      @if (service.data()) {
        <footer appFooter></footer>
      }
    </form>
  `,
  providers: [provideFormService(MyService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class MyPage {
  protected readonly service = injectFormService<MyForm>()
  readonly form = getMyForm(inject(NonNullableFormBuilder))

  constructor() {
    effect(() => {
      const data = this.service.data()
      if (data && this.form.pristine) this.form.reset(data)
    })
  }

  async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
    } else if (await this.service.save(this.form.getRawValue())) {
      this.form.markAsPristine()
    }
  }
}
```

Key details:

- `[formLoading]` is the `Form` directive — applies `TuiForm`, `TuiCardLarge`, and `TuiSkeleton` as host directives
- `provideFormService(MyService)` provides both `MyService` and `FormService` tokens
- Footer's Cancel button triggers `(reset.prevent)` which resets form to last-loaded data
- Form service auto-polls every 5s and exposes `data` as a signal (`undefined` = loading)

### Dialog (add/edit)

```typescript
@Component({
  template: `
    <form tuiForm="m" [formGroup]="form" (submit.prevent)="save()">
      <!-- fields -->
      <footer>
        <button tuiButton appearance="flat" type="button"
          (click)="context.$implicit.complete()">Cancel</button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MyDialog {
  protected readonly context =
    injectContext<TuiDialogContext<ResultType, InputDataType>>()
  protected readonly form = inject(NonNullableFormBuilder).group({ ... })

  save() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }
    this.context.completeWith(this.form.getRawValue())
  }
}
```

Opening from a parent:

```typescript
this.dialogs
  .open<ResultType>(new PolymorpheusComponent(MyDialog), {
    label: 'Edit Thing',
    data: { ... },
  })
  .subscribe(result => { /* handle result */ })
```

### Table

Tables use attribute selectors and `hostDirectives`:

```typescript
@Component({
  selector: '[myItems]',
  host: { class: 'g-table' },
  hostDirectives: [TuiTableDirective],
  template: `
    <thead tuiThead>
      <tr><th tuiTh [sorter]="'name' | tuiSorter">Name</th></tr>
    </thead>
    <tbody>
      @for (item of myItems() | tuiTableSort; track item.id) {
        <tr>
          <td tuiTd>{{ item.name }}</td>
        </tr>
      } @empty {
        <tr>
          <td tuiTd colspan="...">
            <app-placeholder icon="@tui.inbox">No items</app-placeholder>
          </td>
        </tr>
      }
    </tbody>
  `,
})
export class MyTable {
  readonly myItems = input<Item[]>([])
}
```

Row actions use a dropdown menu pattern — see `published-ports/table.ts` for the full example.

### FormService

All data loading/saving goes through `FormService<T>`:

```typescript
@Injectable()
export class MyService extends FormService<MyData> {
  private readonly api = inject(ApiService)

  async load(): Promise<MyData> {
    return this.api.someMethod()
  }
  async store(data: MyData): Promise<void> {
    await this.api.saveSomething(data)
  }
}
```

`FormService` handles: auto-polling (5s via RxJS timer), error toasts, loading state via `data` signal (`undefined` = loading), save with loading indicator via `ActionService`.

### ActionService

Wraps async operations with loading/success/error toasts:

```typescript
await this.actionService.run(
  () => this.api.doSomething(),
  { loading: 'Saving...', restart: true }, // restart: suppresses network errors
)
```

For network-affecting actions (changing LAN IP, restarting services), `restart: true` suppresses transient errors and polls until the device is reachable again.

## API Layer

- **Transport:** JSON-RPC 2.0 over a single HTTP POST endpoint (`/rpc/v1`)
- **Contract:** `ApiService` (abstract) defines all methods. `LiveApiService` calls `RpcService`; `MockApiService` returns static data for dev.
- **Auth:** RPC error code 34 triggers auto-logout. Session cookie with `withCredentials: true`.
- **Mocks:** Toggle via `config.json` → `useMocks`. Dev server runs without a real router.

## Styling Conventions

**No Tailwind.** Uses Taiga CSS variables and global utility classes from `styles.scss`:

| Class                                                                  | Purpose                                            |
| ---------------------------------------------------------------------- | -------------------------------------------------- |
| `g-page`                                                               | Page layout container (flex column, gap)           |
| `g-form`                                                               | Form card (max-width 50rem, field layout rules)    |
| `g-footer`                                                             | Save/Cancel footer row                             |
| `g-table`                                                              | Table wrapper (rounded, neutral bg, sticky header) |
| `g-aside`                                                              | Aside panel content padding                        |
| `g-help`                                                               | Help panel padding rules                           |
| `g-accordion`                                                          | Accordion/details styling                          |
| `g-primary` / `g-secondary` / `g-action` / `g-negative` / `g-positive` | Text color utilities                               |

**Theming:** Dark/light via `tuiTheme` attribute. Custom `'start-9'` appearance for dropdowns and dialogs. All colors use `--tui-*` CSS variables.

**Component styles** are inline in `@Component`. Use SCSS. Keep them minimal — lean on Taiga's built-in styling.

## App Configuration

`app.config.ts` provides:

- `provideZonelessChangeDetection()` — no zone.js
- `provideRouter(routes, { onSameUrlNavigation: 'reload' })`
- Taiga component size overrides: buttons `m`, textfields `m`, forms `m`, badges `m`, cards `compact`, radios `s`, checkboxes `s`
- Custom appearances: `'start-9'` for dropdowns/dialogs, `'neutral'` default
- API service toggle: `MockApiService` or `LiveApiService` based on `config.json`
- HTTP URL: `RELATIVE_URL` token → `/rpc/v1`
