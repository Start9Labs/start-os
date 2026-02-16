# Web — StartWRT Angular Frontend

Admin UI for StartWRT, an OpenWrt-based router. Angular 21 + TypeScript 5.9 + [Taiga UI v5](https://taiga-ui.dev/next) component library. Zoneless change detection, standalone components, signal-based state. No NgModules anywhere.

## Commands

```bash
npm ci                # Install dependencies
npm start             # Dev server (uses config.json to toggle mocks)
npm run build         # Production build
npm run check         # Type-check without emitting
```

`config.json` at the workspace root controls `useMocks` (swap `MockApiService`/`LiveApiService`) and the API endpoint path.

## Golden Rules

1. **Taiga-first.** Use Taiga components, directives, and APIs whenever possible. Avoid hand-rolled HTML/CSS unless nothing in Taiga fits. If Taiga has a component for it, use it.

2. **Pattern-match.** Nearly anything we build has a similar example elsewhere in this codebase. Search for existing patterns before writing new code. Copy the conventions used in neighboring components.

3. **When unsure about Taiga, look it up.** See [Taiga UI Reference](#taiga-ui-reference) below.

## Taiga UI Reference

### llms-full.txt (~2200 lines, all components with code examples)

```
WebFetch url=https://taiga-ui.dev/llms-full.txt prompt="How to use TuiTextfield with a select dropdown"
```

### MCP server (beta) — `taiga-family/taiga-ui-mcp`

Two tools: `get_list_components` (fuzzy search) and `get_component_example` (docs + code for a specific component). Config:

```json
{
  "mcpServers": {
    "taiga-ui": {
      "command": "npx",
      "args": [
        "-y",
        "@anthropic-ai/mcp-remote@latest",
        "https://taiga-ui.dev/next/api/mcp/sse"
      ]
    }
  }
}
```

### When implementing with Taiga

Always **also check this project's code** — we have project-specific wrappers, appearances (`'start-9'`, `'neutral'`), and global size overrides (buttons `m`, textfields `m`, forms `m`, badges `m`, cards `compact`). See `app.config.ts` for the full provider list.

## Project Structure

```
src/app/
├── app.ts                    # Root component (TuiRoot host)
├── app.config.ts             # Providers: Taiga, routing, API, HTTP
├── app.routes.ts             # Top-level routes (auth guard splits login vs home)
│
├── components/               # Shared UI components
│   ├── copy.ts               # Copy-to-clipboard directive
│   ├── footer.ts             # Form footer (Cancel / Save buttons)
│   ├── masked.ts             # Mask sensitive text
│   ├── placeholder.ts        # Empty-state placeholder with icon
│   └── summary.ts            # Read-only display with copy
│
├── directives/
│   ├── form.ts               # [formLoading] — wraps TuiForm + TuiCardLarge + TuiSkeleton
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
│   ├── help.service.ts       # Drives the aside help panel
│   ├── http.service.ts       # Low-level HttpClient.post wrapper
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
│   ├── validators.ts         # Custom validators: ipv4(), ipv6(), prefix()
│   ├── pauseFor.ts           # Promise-based delay
│   ├── languages.ts          # Supported locales
│   └── workspace-config.ts   # Type for config.json
│
└── routes/
    ├── login.ts              # Login page
    └── home/
        ├── index.ts          # Lazy child routes under Outlet
        ├── components/       # Outlet, Header, Nav, Aside
        └── routes/
            ├── wan/          # WAN config (ipv4, ipv6, mac, dns, ddns)
            ├── published-ports/  # Port forwarding — BEST example of table + dialog pattern
            ├── outbound/     # Outbound VPN (WireGuard clients)
            ├── inbound/      # Inbound VPN (WireGuard server + peers)
            ├── ethernet/     # Ethernet port config
            ├── wifi/         # WiFi (passwords, blackout schedule, settings)
            ├── lan/          # LAN settings (ipv4, ipv6)
            ├── devices/      # Device management
            ├── profiles/     # Security profiles
            └── settings/     # General, advanced, password, SSH keys, logs, activity
```

### Route anatomy

Each route folder typically contains:

| File              | Purpose                                      |
| ----------------- | -------------------------------------------- |
| `index.ts`        | Page component (default export, lazy-loaded) |
| `service.ts`      | Extends `FormService<T>` — load/save data    |
| `aside.ts`        | Help content for the aside panel             |
| `dialog.ts`       | Dialog component for add/edit flows          |
| `dialog-aside.ts` | Help content shown inside dialogs            |
| `table.ts`        | Table component (for list views)             |
| `types.ts`        | Local types                                  |
| `uci/service.ts`  | UCI config read/write logic                  |
| `uci/mocks.ts`    | Mock data for dev                            |

## Key Patterns

### Page component (form-based)

```typescript
@Component({
  template: `
    <my-aside *help />
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
  host: { class: 'g-page' },
  providers: [provideFormService(MyService)],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class MyPage {
  protected readonly service = injectFormService<MyForm>()
  readonly form = getMyForm(inject(NonNullableFormBuilder))

  constructor() {
    // Reset form when data loads
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

- `[formLoading]` is our `Form` directive — applies `TuiForm`, `TuiCardLarge`, and `TuiSkeleton` as host directives
- `*help` projects an `<ng-template>` into the aside panel via `HelpService`
- `provideFormService(MyService)` provides both `MyService` and `FormService` tokens
- `injectFormService<T>()` injects the abstract `FormService`
- Footer's Cancel button triggers `(reset.prevent)` which resets form to last-loaded data
- Form service auto-polls every 5s and exposes `data` as a signal (`undefined` = loading)

### Dialog (add/edit)

```typescript
@Component({
  template: `
    <my-dialog-aside *modalHelp />
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

Opening a dialog from a parent:

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
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
        ...
      </tr>
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

Row actions use a dropdown menu pattern — see `published-ports/table.ts` for the full example with enable/disable, edit, and delete actions.

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

`FormService` handles: auto-polling (5s), error toasts, loading state via `data` signal (`undefined` = loading), save with loading indicator via `ActionService`.

## Styling Conventions

**No Tailwind.** Styling uses Taiga CSS variables and global utility classes from `styles.scss`:

| Class                                                                  | Purpose                                            |
| ---------------------------------------------------------------------- | -------------------------------------------------- |
| `g-page`                                                               | Page layout container (flex column, gap)           |
| `g-form`                                                               | Form card (max-width 50rem, field layout rules)    |
| `g-footer`                                                             | Save/Cancel footer row                             |
| `g-table`                                                              | Table wrapper (rounded, neutral bg, sticky header) |
| `g-aside`                                                              | Aside panel content padding                        |
| `g-primary` / `g-secondary` / `g-action` / `g-negative` / `g-positive` | Text color utilities                               |

**Theming:** Dark/light via `tuiTheme` attribute. Custom `'start-9'` appearance for dropdowns and dialogs. All colors use `--tui-*` CSS variables.

**Component styles** are inline in the `@Component` decorator. Use SCSS. Keep them minimal — lean on Taiga's built-in styling.

## API Layer

- **Transport:** JSON-RPC 2.0 over a single HTTP POST endpoint
- **Contract:** `ApiService` (abstract) defines all methods — UCI config CRUD, system ops, VPN management, profiles, etc.
- **Auth:** RPC error code 34 triggers auto-logout
- **UCI:** OpenWrt configuration is read/written via `getUci()`/`setUci()` with strongly typed section interfaces in `api/types.ts`
- **Mocks:** `MockApiService` returns static data for `npm start` development without a real router

## Common Pitfalls

- **Don't use NgModules.** Everything is standalone with `imports: [...]` on the component.
- **Don't use zone.js.** Zoneless change detection is enabled. Use signals and `OnPush`.
- **Don't forget `provideFormService()`** in the page component's `providers` array.
- **Don't hand-roll form cards.** Use the `[formLoading]` directive which bundles `TuiForm` + `TuiCardLarge` + `TuiSkeleton`.
- **Don't create separate `.html` / `.css` files.** Templates and styles are inline in the component decorator.
- **Don't import `FormBuilder`.** Use `NonNullableFormBuilder` for strict typing.
- **Don't forget `changeDetection: ChangeDetectionStrategy.OnPush`** on every component.
