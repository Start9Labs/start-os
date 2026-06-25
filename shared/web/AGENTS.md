# CLAUDE.md

## Operating rules

- **Taiga does it all.** This codebase uses Taiga UI 5 for components, directives, layout, dialogs, forms, icons, and styling. Do not hand-roll HTML/CSS when Taiga provides a solution. If you think Taiga can't do something, you're probably wrong — look it up first.
- **Never guess Taiga APIs.** Taiga 5 has its own component names, directive names, input bindings, and usage patterns. Don't invent them from memory — verify against the docs.
  - Use the `taiga-ui-mcp` MCP server if available — `get_list_components` and `get_component_example` are the fastest path.
  - Otherwise `WebFetch url=https://taiga-ui.dev/llms-full.txt prompt="..."` (~2200 lines covering all components).
  - Taiga docs are authoritative — this project's existing code is not. If they disagree, the docs win, except where this project has a deliberate local wrapper.
- **Follow existing patterns before inventing new ones.** Nearly anything you build has a precedent in this codebase — search for a similar component first and copy its conventions (signals, `inject()`, OnPush, etc. — see ARCHITECTURE.md's component conventions).
- **i18n is mandatory for user-facing strings.** Every English string used in templates goes through `| i18n` and must have an entry in every language dictionary under `shared/src/i18n/dictionaries/`. See ARCHITECTURE.md's i18n section.
- **Use tuiTitle + tuiSubtitle** for a common UI pattern of a vertical stack of primary text and secondary text. Use the <b> tag to make the title bold.
- **`projects/brochure` is a public website, not an embedded OS app.** It's the marketplace front at marketplace.start9.com and **auto-deploys on merge to `master`** (`.github/workflows/deploy-brochure.yml`) — `ui`, `setup-wizard`, and `start-tunnel` ship inside the OS image instead. brochure consumes the same source `shared`/`marketplace` libs as the other apps.

## Taiga 5 idioms to default to

Patterns a senior Taiga refactor corrected in our own code — exactly what "follow existing patterns" gets wrong while the repo is mid-migration. Default to these; verify specifics against the MCP/docs.

- **`tuiProvide(TOKEN, Class)`** (`@taiga-ui/cdk`) — a _type-safe_ wrapper around `{ provide: TOKEN, useExisting: Class }` (checks `Class` satisfies `TOKEN`). Use it for every `useExisting` binding.
- **DI: only root and node injectors.** App-wide singletons at **root** (`app.config.ts`); everything else on the **node** (a component's `providers`/`viewProviders`). **Never add `providers` to routes** — lazy routes spin up confusing semi-root injectors.
- **A component that _is_ a control** (button/link/badge) → attribute selector (`a[myThing]`) + `hostDirectives: [TuiButton]` + static attrs in `host: {}` + config via option providers (`tuiButtonOptionsProvider`, `TUI_ICON_START`), styled on `:host`. Never a `<my-thing>` wrapper around a styled child with a `:host { display: grid }` stretch hack.
- **Layout primitives over CSS**: `tuiHeader`, `tuiForm`, `tuiList`, `tuiAvatar` (large icons — not `tui-icon` + `font-size`), `tuiBadge` (small action chips). Hand-centering a card's contents means you missed a primitive.
- **`appearance="positive|warning|…"`** on components, not `class="g-positive"`. `g-*` color utilities live only in the shared global stylesheet — consolidated, not duplicated per app.
- **Signals**: `[(ngModel)]="signal"`; `linkedSignal({ source, computation })` to auto-reset a signal; optional inputs are `input<T>()` (not `input<T | null>(null)`), and `*ngTemplateOutlet` no-ops on null — no `@if` guard. `protected readonly` for template-used members; static host attrs (`type: 'button'`) in `host: {}`.
