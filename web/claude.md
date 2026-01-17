# OpenWRT Angular GUI Project - Continuation Prompt

## Project Overview

You are working on an Angular-based GUI for OpenWRT routers. The project uses Angular 19+ with standalone components, Taiga UI component library, and communicates with OpenWRT's UCI configuration system.

## Tech Stack

- **Angular 19+**: Standalone components, signals, effects, `toSignal` from `@angular/core/rxjs-interop`
- **Taiga UI**: Component library (TuiTextfield, TuiRadio, TuiSelect, TuiSwitch, TuiHeader, TuiAccordion, etc.)
- **RxJS**: Used sparingly, prefer signals where possible
- **TypeScript**: Strict typing with discriminated unions for UCI sections

## Project Structure

```
src/app/
├── components/          # Shared components (footer, summary)
├── directives/          # Shared directives (form, help)
├── services/
│   ├── api/            # API service, types, mock service
│   └── form.service.ts # Base FormService class
├── utils/
│   └── validators.ts   # CustomValidators (ipv4, ipv6, prefix, mac)
└── routes/home/routes/wan/
    ├── dns/            # Shared DNS component used by IPv4/IPv6
    └── routes/
        ├── ipv4/       # Complete
        ├── ipv6/       # Complete
        ├── mac/        # Complete
        └── ddns/       # Complete
```

## Route Structure Pattern

Each route follows this structure:

```
route-name/
├── index.ts        # Main page component with FormService
├── utils.ts        # Form factory, types, labels, updateValidators function
├── service.ts      # FormService implementation (load/store)
├── summary.ts      # Summary display component
├── aside.ts        # Help sidebar content
├── ip.ts           # (optional) Sub-form component
└── uci/
    ├── service.ts  # UCI read/write logic
    └── mocks.ts    # Mock data for development
```

## Key Patterns

### 1. FormService Pattern

```typescript
// service.ts
@Injectable()
export class RouteService extends FormService<RouteForm> {
  private readonly uci = inject(RouteUciService)
  load() { return this.uci.get() }
  store(data: RouteForm) { return this.uci.set(data) }
}

// index.ts - provide and inject
providers: [provideFormService(RouteService)]
// ...
protected readonly service = injectFormService<RouteForm>()
```

### 2. Form Factory with Dynamic Validators

```typescript
// utils.ts
export function getRouteForm(builder: NonNullableFormBuilder) {
  return builder.group({
    mode: builder.control<Mode>('default'),
    field: builder.control('', [CustomValidators.ipv4()]),
  })
}

export function updateRouteValidators(form: ReturnType<typeof getRouteForm>, mode: Mode): void {
  const { field } = form.controls
  field.clearValidators()
  field.addValidators([CustomValidators.ipv4()])
  if (mode === 'static') {
    field.addValidators([Validators.required])
  }
  field.updateValueAndValidity()
}
```

### 3. Signal Pattern for Form Values

```typescript
// Convert form control to signal with initial value
readonly mode = toSignal(
  this.form.controls.mode.valueChanges.pipe(
    startWith(this.form.controls.mode.value),
  ),
  { requireSync: true },
)

// Effects for data loading and validator updates
constructor() {
  effect(() => {
    const data = this.service.data()
    if (data && this.form.pristine) {
      this.form.reset(data)
      updateRouteValidators(this.form, data.mode)
    }
  })

  effect(() => {
    updateRouteValidators(this.form, this.mode())
  })
}
```

### 4. Index Component Template Pattern

```typescript
template: `
  <route-aside *help />
  <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
  <article routeSummary [formLoading]="!service.data()"></article>
  <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
  <form
    [formGroup]="form"
    [formLoading]="!service.data()"
    (reset.prevent)="form.reset(service.data())"
    (ngSubmit)="onSave()"
  >
    <!-- form content -->
    @if (service.data()) {
      <footer appFooter></footer>
    }
  </form>
`,
host: { class: 'g-page' },
```

### 5. Summary Component Pattern

```typescript
@Component({
  selector: '[routeSummary]',
  template: `
    <section>
      @for (item of items(); track item.label) {
        @if (item.val; as val) {
          <div [appSummary]="val">{{ item.label }}</div>
        }
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class RouteSummary {
  protected readonly service = injectFormService<RouteForm>()
  readonly items = computed(() => {
    /* map data to label/val pairs */
  })
}
```

### 6. UCI Service Pattern

```typescript
type UciFiles = { network: UciFile<UciSection> }

@Injectable({ providedIn: 'root' })
export class RouteUciService {
  private readonly api = inject(ApiService)
  private _uciFiles?: UciFiles

  async get(): Promise<RouteForm> {
    this._uciFiles = await this.api.getUci<UciFiles>({ names: ['network'] })
    // Use type guards for discriminated unions
    const section = this._uciFiles.network.sections.find((s): s is NetworkInterfaceSection => s.type === 'interface' && s.name === 'wan')
    // Transform UCI to form data
  }

  async set(form: RouteForm): Promise<void> {
    const uciFiles = JSON.parse(JSON.stringify(this._uciFiles)) as UciFiles
    // Transform form data to UCI
    await this.api.setUci(uciFiles)
    await this.api.exec({ command: '/etc/init.d/network', args: ['restart'] })
  }
}
```

### 7. UCI Types (discriminated union)

```typescript
// types.ts
export type UciSection = NetworkInterfaceSection | NetworkDeviceSection | DdnsSection

export type NetworkInterfaceSection = {
  type: 'interface'
  name: string | null
  options: { proto: 'dhcp' | 'static' | 'pppoe' | ..., /* fields */ }
  lists: { dns?: string[] }
}
```

### 8. Child Component Accessing Parent

```typescript
// Child injects parent component directly
export class RouteIp {
  protected readonly parent = inject(RouteComponent)
  // Access parent.form, parent.mode(), etc.
}
```

## Mock API

Located at `services/api/mock-api.service.ts`:

- Add mock sections to `mockUci` object
- Import mocks from route's `uci/mocks.ts`

## Styling Notes

- Pages use `host: { class: 'g-page' }`
- Forms use `[formLoading]` directive for skeleton loading
- Summary articles get `min-height: 6rem` during loading
- Aside content uses `host: { class: 'g-aside' }`

## Aside Content Guidelines

- Provide minimum information necessary to understand the screen
- Keep explanations concise and actionable
- Avoid technical jargon unless necessary
- Use accordion (`TuiAccordion`) for multiple sections (IPv4/IPv6)
- Use plain content with `class: 'g-aside'` for simpler pages (MAC/DDNS)

## Important Considerations

1. **Validators on controls, not groups** - Group validators don't show errors in Taiga UI inputs
2. **Always use type guards** with `find()` on UCI sections due to discriminated unions
3. **Prefix fields** use Maskito mask and store value with leading `/` (e.g., `/24`)
4. **Summary shows all populated fields** regardless of mode (ISP may assign values)
5. **Don't show mode/strategy in summary** - only show actual configuration values
6. **TuiDataListWrapper needs `new` attribute** for dropdowns to work
7. **Form reset happens in effect** when `service.data()` loads and form is pristine
