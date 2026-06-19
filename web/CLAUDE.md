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
