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

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the web architecture: API layer, PatchDB state, WebSockets, routing, forms, i18n, and services.

## Component Conventions

- **Standalone components** preferred (no NgModule). Use `imports` array in `@Component`.
- **`export default class`** for route components (enables direct `loadComponent` import).
- **`inject()`** function for DI (not constructor injection).
- **`signal()`** and `computed()`\*\* for local reactive state.
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
<tui-error formControlName="controlName" />
<tui-error [error]="'Error text'" />
<tui-icon [tuiTooltip]="'Hint text'" />
```

### Layout

```html
<tui-elastic-container><!-- dynamic height --></tui-elastic-container>
<tui-scrollbar><!-- scrollable content --></tui-scrollbar>
<tui-loader [textContent]="'Loading...' | i18n" />
```
