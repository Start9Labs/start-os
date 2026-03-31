# StartOS Web

[Angular](https://angular.dev/) + TypeScript workspace using the [Taiga UI](https://taiga-ui.dev/) component library.

## Applications

StartOS serves one of these UIs depending on the state of the system:

- **ui** — Primary admin interface for managing StartOS, served on hosts unique to the instance.
- **setup-wizard** — Initial setup UI, served on `start.local`.
- **start-tunnel** — VPN/tunnel management UI.

## Libraries

- **shared** — Common code shared between all web UIs (API clients, components, i18n).
- **marketplace** — Library code for service discovery, shared between the StartOS UI and the marketplace.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for environment setup, development server instructions, and translation guides.
