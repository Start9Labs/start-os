# Container Runtime Architecture

The container runtime is a Node.js/TypeScript program that runs as a long-lived process inside every StartOS service container. It loads the service's JavaScript from the s9pk and manages subcontainers; the StartOS host daemon talks to it over a Unix socket.

```
LXC Container (uniform base for all services)
└── systemd
    └── container-runtime.service
        └── Loads /usr/lib/startos/package/index.js (from s9pk javascript.squashfs)
            └── Package JS launches subcontainers (from images in s9pk)
```

The container runtime communicates with the host via JSON-RPC over Unix socket. Package JavaScript must export functions conforming to the `ABI` type defined in `sdk/base/lib/types.ts`.

## `/media/startos/` Directory (mounted by host into container)

| Path                 | Description                                           |
| -------------------- | ----------------------------------------------------- |
| `volumes/<name>/`    | Package data volumes (id-mapped, persistent)          |
| `assets/`            | Read-only assets from s9pk `assets.squashfs`          |
| `images/<name>/`     | Container images (squashfs, used for subcontainers)   |
| `images/<name>.env`  | Environment variables for image                       |
| `images/<name>.json` | Image metadata                                        |
| `backup/`            | Backup mount point (mounted during backup operations) |
| `rpc/service.sock`   | RPC socket (container runtime listens here)           |
| `rpc/host.sock`      | Host RPC socket (for effects callbacks to host)       |

## RPC Protocol

The runtime listens on `/media/startos/rpc/service.sock` and exposes a JSON-RPC API for the host (`init`, `exit`, package action calls, etc.). The full method list and parameter types live in [`RPCSpec.md`](RPCSpec.md).

## S9PK Structure

See [`../core/s9pk-structure.md`](../core/s9pk-structure.md) for the S9PK package format, which determines what the container runtime loads at startup.
