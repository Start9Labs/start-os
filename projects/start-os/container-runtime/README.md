# Container Runtime

Node.js/TypeScript runtime that runs inside each StartOS service's LXC container. It loads the service's JavaScript out of its s9pk package, manages subcontainers, and brokers JSON-RPC calls between the service and the StartOS host daemon.

Every service container boots from the same base image; the per-service behavior comes entirely from the JavaScript loaded out of `javascript.squashfs` inside the s9pk and from container images mounted under `/media/startos/`.

This is a sub-component of the **start-os** product in the Start9 monorepo. The host-side daemon that drives it lives in `shared-libs/crates/start-core` (the `start_core` Rust library); the `start-container` binary that supervises this process is built from package `start-os`.

## Where it fits

```
start-os/
├── src/bin/            startbox + start-container (Rust)
├── container-runtime/  <- you are here (Node service runtime)
├── web/                Angular UI + setup wizard
└── ...
shared-libs/crates/start-core/   host daemon library (talks to this over a socket)
projects/start-sdk/         @start9labs/start-sdk (this package's runtime API)
```

## Documentation

- `ARCHITECTURE.md` — runtime topology, the host/container RPC boundary, and the `/media/startos/` mount layout
- `CONTRIBUTING.md` — local build, type-check, and test workflow
- `AGENTS.md` — practical build/test instructions and gotchas (also imported by `CLAUDE.md`)
- `RPCSpec.md` — full JSON-RPC protocol the runtime exposes over `service.sock`

## Quickstart

The runtime depends on the **built** SDK, referenced as `file:../../start-sdk/dist`, so build the SDK first:

```bash
cd projects/start-sdk && make bundle && cd -      # produces start-sdk/dist
npm --prefix projects/start-os/container-runtime ci
npm --prefix projects/start-os/container-runtime run check   # type-check
npm --prefix projects/start-os/container-runtime test        # jest
```

From the monorepo root you can also drive it through the top-level Makefile:

```bash
make test-container-runtime              # build SDK + run jest
```
