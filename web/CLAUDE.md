# Web — Angular Frontend

Angular 21 + TypeScript workspace using [Taiga UI 5](https://taiga-ui.dev/) component library.

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

1. **Taiga does it all.** We use Taiga UI 5 for everything — components, directives, layout, dialogs, forms, icons, and styling. Do not hand-roll HTML/CSS when Taiga provides a solution. If you think Taiga can't do something, you're probably wrong — look it up first.

2. **Follow existing patterns.** Before writing new code, search this codebase for a similar example. Nearly anything we build has a precedent. Copy the conventions used in neighboring components. Do not invent new patterns when established ones exist.

3. **Never guess Taiga APIs.** Taiga UI 5 has its own way of doing things. Do not make up component names, directive names, input bindings, or usage patterns from memory. Always verify against the official docs or the MCP server. Getting it wrong wastes everyone's time.

4. **Use the Taiga MCP server.** If a `taiga-ui-mcp` MCP server is available, use it to look up components and get documentation with code examples. It provides two tools: `get_list_components` (search/filter components) and `get_component_example` (get full docs and examples for a component). This is the fastest and most accurate way to get Taiga usage information.

5. **Fall back to the Taiga docs.** If the MCP server is not available, use `WebFetch` against `https://taiga-ui.dev/llms-full.txt` to search for component usage. Taiga docs are authoritative — this project's code is not. See [Taiga UI Docs](#taiga-ui-docs) below.

## Taiga UI Docs

Taiga provides AI-friendly references at [taiga-ui.dev/ai-support](https://taiga-ui.dev/ai-support):

- **MCP server** — [`taiga-ui-mcp`](https://github.com/taiga-family/taiga-ui-mcp) provides full access to Taiga UI component docs and Angular code examples via the Model Context Protocol.
- **llms-full.txt** — `https://taiga-ui.dev/llms-full.txt` (~2200 lines covering all components with code examples). Use `WebFetch` to search it:

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
