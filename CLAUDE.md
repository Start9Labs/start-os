# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full system architecture, component map, build pipeline, and cross-layer verification order.

Each major component has its own `CLAUDE.md` with detailed guidance: `core/`, `web/`, `container-runtime/`, `sdk/`.

## Build & Development

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Environment setup and requirements
- Build commands and make targets
- Testing and formatting commands
- Environment variables

**Quick reference:**
```bash
. ./devmode.sh                            # Enable dev mode
make update-startbox REMOTE=start9@<ip>   # Fastest iteration (binary + UI)
make test-core                            # Run Rust tests
```

## Operating Rules

- Always verify cross-layer changes using the order described in [ARCHITECTURE.md](ARCHITECTURE.md#cross-layer-verification)
- Check component-level CLAUDE.md files for component-specific conventions
- Follow existing patterns before inventing new ones

## Supplementary Documentation

The `docs/` directory contains cross-cutting documentation for AI assistants:

- `TODO.md` - Pending tasks for AI agents (check this first, remove items when completed)
- `USER.md` - Current user identifier (gitignored, see below)
- `exver.md` - Extended versioning format (used across core, sdk, and web)
- `VERSION_BUMP.md` - Guide for bumping the StartOS version across the codebase

Component-specific docs live alongside their code (e.g., `core/rpc-toolkit.md`, `core/i18n-patterns.md`).

### Session Startup

On startup:

1. **Check for `docs/USER.md`** - If it doesn't exist, prompt the user for their name/identifier and create it. This file is gitignored since it varies per developer.

2. **Check `docs/TODO.md` for relevant tasks** - Show TODOs that either:
   - Have no `@username` tag (relevant to everyone)
   - Are tagged with the current user's identifier

   Skip TODOs tagged with a different user.

3. **Ask "What would you like to do today?"** - Offer options for each relevant TODO item, plus "Something else" for other requests.
