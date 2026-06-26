# AGENTS.md

Agent/dev instructions for `shared-libs/ts-modules` — the Angular workspace root and the two shared libs (`@start9labs/shared`, `@start9labs/marketplace`). See `ARCHITECTURE.md` for structure, `CONTRIBUTING.md` for full setup.

## Where things are

- **You are at the workspace root.** `angular.json`, `package.json`, `tsconfig.json` all live here. The libs `shared/` and `marketplace/` live here directly.
- **Apps live elsewhere.** `ui` → `../../start-os/web/ui`, `setup-wizard` → `../../start-os/web/setup-wizard`, `start-tunnel` → `../../start-tunnel/web`, `brochure` → `../../brochure`. Editing app code means editing those dirs even though `ng`/`tsc` are run from here.
- i18n dictionaries: `shared/src/i18n/dictionaries/`.

## Build / test / check (run from `shared-libs/ts-modules`)

```sh
npm ci
npm run build:deps           # MUST run first after install — builds the file: deps (start-sdk, patch-db client)
npm run check                # type-check all projects; or check:shared / check:ui / etc. for one
npm run format               # prettier; format:check for CI
npm run start:ui             # mock dev server (needs config.json — cp config-sample.json)
npm run build:ui             # prod build of a single app
```

Gotchas:

- `@start9labs/start-sdk` and `patch-db-client` are `file:` deps built by `build:deps`; a fresh checkout won't type-check until you run it.
- There is no unit-test runner wired up — `npm run check` (tsc, strict + strictTemplates) plus a successful `build:*` is the verification bar.
- `shared-libs/crates/patch-db` is a first-party crate; `build:deps` runs `npm ci && npm run build` inside it.

## Operating rules

- **Taiga does it all.** This codebase uses Taiga UI 5 for components, directives, layout, dialogs, forms, icons, and styling. Do not hand-roll HTML/CSS when Taiga provides a solution. If you think Taiga can't do something, you're probably wrong — look it up first.
- **Never guess Taiga APIs.** Taiga 5 has its own component names, directive names, input bindings, and usage patterns. Don't invent them from memory — verify against the docs.
  - Use the `taiga-ui-mcp` MCP server if available — `get_list_components` and `get_component_example` are the fastest path.
  - Otherwise `WebFetch url=https://taiga-ui.dev/llms-full.txt prompt="..."` (~2200 lines covering all components).
  - Taiga docs are authoritative — this project's existing code is not. If they disagree, the docs win, except where this project has a deliberate local wrapper.
- **Follow existing patterns before inventing new ones.** Nearly anything you build has a precedent in this codebase — search for a similar component first and copy its conventions (signals, `inject()`, OnPush, etc. — see ARCHITECTURE.md's component conventions).
- **i18n is mandatory for user-facing strings.** Every English string used in templates goes through `| i18n` and must have an entry in every language dictionary under `shared/src/i18n/dictionaries/`. See ARCHITECTURE.md's i18n section.
- **Use tuiTitle + tuiSubtitle** for a common UI pattern of a vertical stack of primary text and secondary text. Use the <b> tag to make the title bold.
- **`brochure` (`../../brochure`) is a public website, not an embedded OS app.** It's the marketplace front at marketplace.start9.com and **auto-deploys on merge to `master`** (`.github/workflows/deploy-brochure.yml`) — `ui`, `setup-wizard`, and `start-tunnel` ship inside the OS image instead. brochure consumes the same source `shared`/`marketplace` libs as the other apps.

## Taiga 5 idioms to default to

Patterns a senior Taiga refactor corrected in our own code — exactly what "follow existing patterns" gets wrong while the repo is mid-migration. Default to these; verify specifics against the MCP/docs.

- **`tuiProvide(TOKEN, Class)`** (`@taiga-ui/cdk`) — a _type-safe_ wrapper around `{ provide: TOKEN, useExisting: Class }` (checks `Class` satisfies `TOKEN`). Use it for every `useExisting` binding.
- **DI: only root and node injectors.** App-wide singletons at **root** (`app.config.ts`); everything else on the **node** (a component's `providers`/`viewProviders`). **Never add `providers` to routes** — lazy routes spin up confusing semi-root injectors.
- **A component that _is_ a control** (button/link/badge) → attribute selector (`a[myThing]`) + `hostDirectives: [TuiButton]` + static attrs in `host: {}` + config via option providers (`tuiButtonOptionsProvider`, `TUI_ICON_START`), styled on `:host`. Never a `<my-thing>` wrapper around a styled child with a `:host { display: grid }` stretch hack.
- **Layout primitives over CSS**: `tuiHeader`, `tuiForm`, `tuiList`, `tuiAvatar` (large icons — not `tui-icon` + `font-size`), `tuiBadge` (small action chips). Hand-centering a card's contents means you missed a primitive.
- **`appearance="positive|warning|…"`** on components, not `class="g-positive"`. `g-*` color utilities live only in the shared global stylesheet — consolidated, not duplicated per app.
- **Signals**: `[(ngModel)]="signal"`; `linkedSignal({ source, computation })` to auto-reset a signal; optional inputs are `input<T>()` (not `input<T | null>(null)`), and `*ngTemplateOutlet` no-ops on null — no `@if` guard. `protected readonly` for template-used members; static host attrs (`type: 'button'`) in `host: {}`.
