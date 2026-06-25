# Contributing

We welcome contributions! Whether you spot a typo or want to suggest new content, fork this repo and submit a PR. If you're not comfortable with that process, [create an issue](https://github.com/Start9Labs/docs/issues) or reach out via our [community channels](https://start9.com/contact).

## Documentation

This repo's docs split across:

- `README.md` — what this is
- `ARCHITECTURE.md` — how it's built
- `CONTRIBUTING.md` — this file; how to contribute
- `CLAUDE.md` — AI-developer operating rules

**These docs must be kept up to date.** When you change project structure, conventions, build process, or product context, update the relevant file(s) in the same change — do not defer.

## Local Setup

1. Install [Rust](https://rustup.rs):

   ```
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Install [mdBook](https://rust-lang.github.io/mdBook/) and [mdbook-tabs](https://github.com/niccoloforlini/mdbook-tabs):

   ```
   cargo install mdbook mdbook-tabs
   ```

3. Install [Node.js](https://nodejs.org/) (v22+) and script dependencies:

   ```
   cd scripts && npm install && cd ..
   ```

4. Clone and serve:

   ```
   git clone https://github.com/Start9Labs/docs.git && cd docs
   ./serve.sh
   ```

   This builds all books and serves at http://localhost:3000. For live-reload while editing a single book:

   ```
   cd start-os && mdbook serve -p 3001
   ```

## Build & Tooling

- `./build.sh` — build all books into `docs/`
- `./serve.sh` — build + serve on http://localhost:3000
- `cd <book> && mdbook serve -p 3001` — live-reload a single book
- `cd scripts && npm run generate-llms-txt` — regenerate `llms.txt` files

## Writing Docs

All documentation lives under `start-os/src/`, `start-tunnel/src/`, `packaging/src/`, or `bitcoin-guides/src/` as flat Markdown files (no subdirectory nesting). The sidebar for each book is defined by its `src/SUMMARY.md`, which uses `# Part Title` lines to create section headers and `---` for visual separators.

### Page Structure

Every page should have introductory prose (1–2 sentences) between the H1 heading and the first H2. This text is auto-extracted for `llms.txt` to help AI decide which pages to fetch. When creating a new page, add it to the book's `src/SUMMARY.md` or it won't appear in the sidebar or build.

### Admonitions

Use mdBook's built-in admonition syntax:

```markdown
> [!WARNING]
> Do not do this.

> [!NOTE]
> Something helpful.

> [!TIP]
> A useful suggestion.
```

Custom titles are **not supported** — `> [!WARNING] My Title` will break. Put context in the body instead.

### Tabs

Use mdbook-tabs for platform-specific content:

```markdown
{{#tabs global="platform"}}
{{#tab name="Mac"}}
Mac instructions here.
{{#endtab}}
{{#tab name="Linux"}}
Linux instructions here.
{{#endtab}}
{{#endtabs}}
```

`global="..."` makes a tab group's selection sticky across pages (via `localStorage`, mirrored to a `?<global>=<tab>` URL param). Every group sharing a `global` name **must use identical tab labels** — a stored label that isn't present in a group is silently ignored, so mismatched sets fail to sync. So:

- OS pickers use `global="platform"` with exactly these labels: `Mac`, `Windows`, `Linux`, `iOS`, `Android / Graphene`. Put distro/version specifics in `####` sub-sections within a tab, not in extra tabs.
- Anything that isn't a general OS picker (backup targets, cloud providers, …) gets its own `global` — don't overload `platform`.
- Omit `global` for a one-off, page-local group.

Avoid nesting tabs — use separate sections with single-level tabs instead.

### Cross-Book Links

Links between books use absolute paths since mdBook only validates intra-book links:

```markdown
See the [StartTunnel docs](/start-tunnel/).
```

Within a book, use relative paths as usual.

## Submitting Changes

1. Fork and create a branch
2. Make your changes
3. Submit a PR against `master`
