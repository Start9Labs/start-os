# Start9 Docs

The official documentation for [Start9](https://start9.com) products — covering StartOS, StartTunnel, Service Packaging, and Bitcoin Guides.

**Live site:** [docs.start9.com](https://docs.start9.com)

## Books

| Product | URL | Content |
|---------|-----|---------|
| [StartOS](start-os/) | [docs.start9.com/start-os](https://docs.start9.com/start-os/) | Setup, services, networking, backups, system admin, firmware |
| [StartTunnel](start-tunnel/) | [docs.start9.com/start-tunnel](https://docs.start9.com/start-tunnel/) | Installation, subnets, devices, port forwarding, architecture |
| [Service Packaging](packaging/) | [docs.start9.com/packaging](https://docs.start9.com/packaging/) | Developer guide for building and publishing StartOS services |
| [Bitcoin Guides](bitcoin-guides/) | [docs.start9.com/bitcoin-guides](https://docs.start9.com/bitcoin-guides/) | Guides for running Bitcoin and related services on StartOS |

### Product context

- **StartOS 0.4.0** — networking strategies that aren't built into StartOS core (Tor, P2P tunnels, etc.) are distributed as marketplace services. Outbound gateways support a system default plus per-service override.
- **Service Packaging** — these docs are the single source of truth for service packaging. Code examples are based on the [hello-world](https://github.com/Start9Labs/hello-world-startos) template.
- **StartTunnel** — a WireGuard-based gateway service for clearnet access. Separate product, separate book.
- **Bitcoin Guides** — wallet indexes, Electrum servers, archival vs pruned nodes, LND migration. The Bitcoin package on StartOS integrates btc-rpc-proxy for on-demand block fetching, so pruned nodes work transparently with multiple downstream services.

## How It Works

Built with [mdBook](https://rust-lang.github.io/mdBook/) and deployed via rsync to a VPS. Each product is an independent mdBook instance sharing a common theme.

On push to `master`, GitHub Actions:
1. Builds all books into versioned `docs/` output directories
2. Generates `llms.txt` and `llms-full.txt` for LLM consumption
3. Deploys to the VPS via rsync and reloads nginx

## Getting Started

See [CONTRIBUTING.md](CONTRIBUTING.md) for local development setup and how to submit changes.

See [ARCHITECTURE.md](ARCHITECTURE.md) for how the repo is structured and why.
