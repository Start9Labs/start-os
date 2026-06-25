# CLAUDE.md

For AI developers (Claude Code, Copilot, etc.). See `CONTRIBUTING.md` for the doc map and contribution workflow.

## Operating rules

- Always re-read a file before making subsequent edits — a linter/formatter auto-modifies files after changes.
- Never use custom admonition titles. `> [!WARNING] Custom Title` is broken in mdBook. Always use plain `> [!WARNING]` and put context in the body.
- Avoid nested tabs. Use separate sections with single-level tabs.
- Cross-book links must use absolute paths (`/start-tunnel/devices.html`), not relative paths.
- All pages are flat in `src/` — no subdirectory nesting. Sidebar sections use `# Part Title` in SUMMARY.md.
- Every page should have introductory prose between the H1 heading and the first H2. This text is auto-extracted for `llms.txt` to help AI decide which pages to fetch.
- When creating a new page, add it to the book's `src/SUMMARY.md` or it won't appear in the sidebar or build.
- When adding a new book, add a single line to `versions.conf` — build, deploy, and nginx routing all derive from it. No other config files need updating.
