# assets/

Supplementary files that ship inside the `.s9pk` alongside your compiled code —
anything the service needs at runtime that isn't part of the upstream image.
Typical contents:

- entrypoint or wrapper scripts the daemon execs,
- configuration templates / generators,
- static files (default configs, seed data, certificates),
- an `ABOUT.md` or other docs you want bundled.

To make these files visible inside a container, mount them in `startos/main.ts`
with `sdk.Mounts.…mountAssets(…)` and exec against the mountpoint (see the
Volume Mounts section of `../../src/main.md`).

This directory is **required and must not be empty** — the build fails and git
won't track an empty directory. This README satisfies that requirement, so don't
delete it unless you're adding real assets to replace it.
