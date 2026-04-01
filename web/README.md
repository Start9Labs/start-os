# Web — StartWRT Admin UI

Single-page application for managing the StartWRT router. Angular 21, TypeScript 5.9, Taiga UI v5. Signal-based state, zoneless change detection, standalone components.

Communicates with the backend over JSON-RPC 2.0 at `/rpc/v1`. Every page includes contextual help via a collapsible aside panel.

## Quick Start

```bash
npm ci          # Install dependencies
npm start       # Dev server (mock API by default)
npm run build   # Production build
npm run check   # Type-check without emitting
```

The dev server uses mock data by default (`config.json` → `useMocks: true`), so no router or running backend is needed.

## Feature Areas

| Section               | Routes                                       | Description                                                   |
| --------------------- | -------------------------------------------- | ------------------------------------------------------------- |
| **Internet**          | WAN Settings, Published Ports, Outbound VPNs | External connectivity, port forwarding, VPN clients           |
| **Network**           | LAN Settings, Devices                        | Local network config and device management                    |
| **Security Profiles** | Profiles                                     | Create and manage access control profiles                     |
| **Points of Entry**   | Ethernet, Wi-Fi, Inbound VPNs                | How devices join the network and get profiled                 |
| **System**            | Settings                                     | General, advanced, password, SSH keys, logs, activity, backup |

## Documentation

- [ARCHITECTURE.md](ARCHITECTURE.md) — Frontend internals: project structure, patterns, styling, API layer
- [CONTRIBUTING.md](CONTRIBUTING.md) — Development guide: setup, adding routes, Taiga UI lookup
- [CLAUDE.md](CLAUDE.md) — AI assistant quick reference
