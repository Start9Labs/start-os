# StartOS Packaging — Agent Context

You are an AI assistant working in a **StartOS packaging workspace**. You help create, maintain, and update `.s9pk` service packages for StartOS. This file is your always-on context: the rules to follow, the patterns to know, and a map of where to read for any given task. The substance lives in the packaging guide under `start-docs/` — read those pages locally, on demand, as the task requires. Do not load everything at once.

## Workspace layout

```
<workspace>/
├── .startos/              ← workspace marker: build-key (signs your packages) + config.yaml (hosts, registries)
├── AGENTS.md              ← this file (symlink → start-docs/packaging/AGENTS.md)
├── AGENTS.local.md        ← your workspace-specific notes (never overwritten by a sync)
├── CLAUDE.md              ← loads AGENTS.md + AGENTS.local.md (Claude Code bridge)
├── start-docs/            ← the packaging guide + package template (read it locally)
└── <id>-startos/ …        ← one or more package repos
```

Each package repo holds: `README.md` (what it is / how it differs from upstream), `instructions.md` (end-user docs shown in StartOS), `UPDATING.md` (upstream-version tracking), `TODO.md` (pending work), and `startos/` (the SDK code).

## Keeping the workspace current

The guide, the package template, and this file all live in `start-docs/`, so syncing it refreshes everything at once. **At the start of every session, fast-forward it against its remote:**

```
git -C start-docs pull --ff-only
```

To track a different source (e.g. a fork), repoint `start-docs`'s remote first — the sync follows whatever remote is configured. Keep workspace-specific notes in `AGENTS.local.md`; a sync never touches it.

## How to use the guide (local-first)

The guide has two layers:

- **Recipes** — intent-driven pages: *what* to do and *which* constructs to combine. **Start here.** Each recipe names the SDK APIs and files involved and links to the reference pages and to real packages.
- **Reference** — concept pages documenting each SDK construct in depth with code examples.

Workflow for any task:

1. **Find the recipe.** Open the intent index: `start-docs/packaging/src/recipes.md`.
2. **Follow its reference links** for API details and code examples.
3. **Follow its package links** — read the specific files it names in a real package (`startos/main.ts`, `startos/actions/`, …) for working production code.
4. **Read only what the task needs.**

Read pages from your local checkout (`start-docs/packaging/src/<page>.md`). Only if `start-docs/` is missing, fall back to the web (`https://docs.start9.com/packaging/<page>.html`).

## Where to read for X

| Need | Read |
| --- | --- |
| Find the right recipe for a task | `start-docs/packaging/src/recipes.md` |
| How to behave on every task (the disciplines below) | `start-docs/packaging/src/workflow.md` |
| File/directory layout of a package | `start-docs/packaging/src/project-structure.md` |
| Service metadata, descriptions | `start-docs/packaging/src/manifest.md` |
| Versions, migrations, release notes | `start-docs/packaging/src/versions.md` |
| Daemons, health checks, oneshots, lifecycle | `start-docs/packaging/src/main.md` |
| Install / update / restore init logic | `start-docs/packaging/src/init.md` |
| Network interfaces and ports | `start-docs/packaging/src/interfaces.md` |
| User-facing actions | `start-docs/packaging/src/actions.md` |
| Prompting the user to run actions | `start-docs/packaging/src/tasks.md` |
| Config files as typed models | `start-docs/packaging/src/file-models.md` |
| Service dependencies | `start-docs/packaging/src/dependencies.md` |
| Build / install commands | `start-docs/packaging/src/makefile.md` |
| Writing the README | `start-docs/packaging/src/writing-readmes.md` |
| Writing user instructions | `start-docs/packaging/src/writing-instructions.md` |
| Publishing / registries | `start-docs/packaging/src/publishing.md` |
| `start-cli` reference | `start-docs/packaging/src/cli.md` |

## Key patterns

Understand these before writing any code (full detail on the pages above):

- **Daemons** define the containers that run your service — subcontainer, exec command, `ready` health check, and a `requires` array for startup ordering. (`main.md`)
- **Oneshots** run a command to completion before dependent daemons start — file ownership (`chown`), migrations, wallet unlocks, config generation. Chained with `.addOneshot()` alongside `.addDaemon()` in `setupMain()`. (`recipe-oneshot.md`, `main.md`)
- **Health checks** come in two forms: the `ready` property on every daemon, and standalone `.addHealthCheck()` calls for ongoing conditions (sync progress, reachability). (`main.md`)
- **runUntilSuccess** spins up a temporary daemon chain during install to bootstrap a service through its own API, then tears it down. (`recipe-run-until-success.md`)
- **File models** are zod-typed representations of config files (JSON, YAML, TOML, …) providing defaults, validation, and reactive reads — the backbone of configuration. (`file-models.md`)

## Golden rules

- **Start from intent, not from API.** Find the recipe before diving into reference pages.
- **Code lives in reference pages and packages, not recipes.** Recipes describe the pattern; reference pages have the API; real packages have production implementations.
- **Match existing patterns.** All packages follow identical conventions — read a package's code before introducing a new pattern.

## Working discipline (every task)

The full rules are in `start-docs/packaging/src/workflow.md`; this is the digest.

- **Verify facts; don't assert from memory.** Image names, tags, version numbers, config formats, credential schemes — confirm each with a tool before you rely on it. "I know that X" is a cue to check X, not to write it down. Guessing an image that doesn't exist or a password format the app rejects fails silently.
- **Compiling is not working.** A green `tsc` and a clean `s9pk pack` prove the code builds, not that the service runs. Before reporting a feature done, exercise it against a running service (install, log in, write data, restart). State what you verified and what you didn't — never imply a feature works when you only compiled it.
- **Don't fabricate; verify or flag.** Never ship an invented icon/logo, a config format you didn't confirm, or placeholder facts in the README. Fetch the real thing, or leave it and flag the gap in `TODO.md`.
- **Search before declaring impossible.** Before working around a limitation, grep the SDK types (`node_modules/@start9labs/start-sdk/**/*.d.ts`) and existing packages. "The SDK can't do X" is a claim to verify in the types, not a conclusion from the docs (this is how `runAsInit` is found).
- **Keep `README.md` and `instructions.md` in sync.** `README.md` tracks architecture/behavior (for developers and AI); `instructions.md` tracks user-visible changes — update each in the same change as the code. Content rules: `writing-readmes.md`, `writing-instructions.md`.
- **Iterate with a dirty tree; commit once.** The `-modified` pack-hash suffix is informational — don't commit between test attempts. One clean commit when the package works; `git reset --soft HEAD~N` collapses accumulated fixups.
- **Pre-existing errors are still errors.** A red `tsc`, test, or pack step means the package doesn't pass, even if unrelated to your change. Fix it or flag it; never report green when a check was red.
- **Don't create unnecessary version files.** The latest version always lives in `startos/versions/current.ts`; most bumps just edit that file in place. A new file is spun off only when the bump carries a migration. See `versions.md` (When to Create a New Version File, Release Notes).

## Starting a new package

**Scaffold first — run `start-cli s9pk init-package "<Name>"`. Do not hand-assemble a package by copying files out of another one.** Scaffolding produces a barebones hello-world clone with a `TODO.md` checklist. **Then work `TODO.md` top to bottom** — it takes the package from clone to release-ready (descriptions, image, icon, interfaces, daemons, docs, first build, install-and-verify). Keep it as the live worklist: remove items as you complete them, add items when you defer work. Wrapping an existing upstream Docker image? Read `recipe-prebuilt-image.md` first.
