# Web — StartWRT Angular Frontend

Admin UI for StartWRT, an OpenWrt-based router. Angular 21 + TypeScript 5.9 + [Taiga UI v5](https://taiga-ui.dev/next) component library. Zoneless change detection, standalone components, signal-based state. No NgModules anywhere.

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full frontend architecture: project structure, route anatomy, key patterns (page, dialog, table, FormService), styling, API layer.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup, config.json, and how-to guides.

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

Always **also check this project's code** — we have project-specific wrappers, appearances (`'start-9'`, `'neutral'`), and global size overrides (buttons `m`, textfields `m`, forms `m`, badges `m`, cards `compact`, radios `s`, checkboxes `s`). See `app.config.ts` for the full provider list.

## Operating Rules

- `[formLoading]` is the `Form` directive in `components/form.ts` — applies `TuiForm`, `TuiCardLarge`, and `TuiSkeleton` as host directives
- Help directives (`*help`, `*modalHelp`) live in `help/`, not `components/`
- `provideFormService(MyService)` provides both `MyService` and `FormService` tokens
- Footer's Cancel button triggers `(reset.prevent)` to reset form to last-loaded data
- Always use `NonNullableFormBuilder`, never `FormBuilder`
- Always set `changeDetection: ChangeDetectionStrategy.OnPush`
- Templates and styles are inline in `@Component` — no separate `.html` / `.css` files
- Styling uses Taiga CSS variables and global `g-*` utility classes — no Tailwind

## Key Files

| File                          | Purpose                                                        |
| ----------------------------- | -------------------------------------------------------------- |
| `app.config.ts`               | All providers: Taiga, routing, API toggle, size overrides      |
| `app.routes.ts`               | Auth guards (setup → initial password → authenticated → login) |
| `services/api/api.service.ts` | Abstract API contract (all RPC methods)                        |
| `services/form.service.ts`    | `FormService<T>` — load/save/refresh with auto-polling         |
| `services/action.service.ts`  | Async ops with loading/success/error toasts                    |
| `components/form.ts`          | `[formLoading]` directive                                      |
| `help/help.ts`                | `*help` directive for aside panel                              |
| `routes/published-ports/`     | Best example of table + dialog pattern                         |
