# Container Runtime — Node.js Service Manager

Node.js runtime that manages service containers via JSON-RPC. See `RPCSpec.md` in this directory for the full RPC protocol.

## Architecture

```
LXC Container (uniform base for all services)
└── systemd
    └── container-runtime.service
        └── Loads /usr/lib/startos/package/index.js (from s9pk javascript.squashfs)
            └── Package JS launches subcontainers (from images in s9pk)
```

The container runtime communicates with the host via JSON-RPC over Unix socket. Package JavaScript must export functions conforming to the `ABI` type defined in `sdk/base/lib/types.ts`.

## `/media/startos/` Directory (mounted by host into container)

| Path | Description |
|------|-------------|
| `volumes/<name>/` | Package data volumes (id-mapped, persistent) |
| `assets/` | Read-only assets from s9pk `assets.squashfs` |
| `images/<name>/` | Container images (squashfs, used for subcontainers) |
| `images/<name>.env` | Environment variables for image |
| `images/<name>.json` | Image metadata |
| `backup/` | Backup mount point (mounted during backup operations) |
| `rpc/service.sock` | RPC socket (container runtime listens here) |
| `rpc/host.sock` | Host RPC socket (for effects callbacks to host) |

## S9PK Structure

See `../core/s9pk-structure.md` for the S9PK package format.
