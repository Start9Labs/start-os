# Container Runtime

Node.js runtime that runs inside each StartOS service's LXC container. Loads the service's JavaScript from its s9pk package, manages subcontainers, and brokers JSON-RPC calls between the service and the StartOS host daemon.

Each LXC container has the same base image; the per-service behavior comes from JavaScript loaded out of `javascript.squashfs` inside the s9pk and from images mounted under `/media/startos/`.

## Documentation

- `ARCHITECTURE.md` — runtime topology, host/container RPC boundary, `/media/startos/` mount layout
- `CONTRIBUTING.md` — local build, type-check, and test workflow
- `RPCSpec.md` — full JSON-RPC protocol the runtime exposes over `service.sock`
- `../core/s9pk-structure.md` — S9PK package format
