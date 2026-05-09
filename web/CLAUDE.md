# CLAUDE.md

## Operating rules

- **Taiga-first.** Use Taiga components, directives, and APIs whenever possible. Don't hand-roll HTML/CSS unless nothing in Taiga fits.
- **When unsure about a Taiga API, look it up.** Don't guess component names, inputs, or directives — see CONTRIBUTING.md § Taiga UI Lookup for the WebFetch URL and MCP server config.
- **Pattern-match.** Search the codebase for an analogous existing route/component before writing new code. Copy the conventions used in neighbours (e.g. `routes/published-ports/` for the table + dialog pattern).
- **No test framework is wired up here** (no Jest/Karma/Vitest in `package.json`). Don't add one without asking.
