# Contributing

## Tech Stack

- **Angular 21** with zoneless change detection and standalone components
- **TypeScript 5.9** in strict mode
- **[Taiga UI v5](https://taiga-ui.dev/next)** component library
- **Signal-based state management** (no external store library)
- **SCSS** for styling, using Taiga CSS variables for theming

## Getting Started

```bash
cd web
npm ci                # Install dependencies
npm start             # Dev server with mock API
npm run build         # Production build
npm run check         # Type-check without emitting
```

The dev server uses a mock API by default (`config.json` → `useMocks: true`), so no real router is needed during development.

## Architecture & Patterns

See [CLAUDE.md](CLAUDE.md) for detailed architecture documentation, project structure, code patterns, and conventions.
