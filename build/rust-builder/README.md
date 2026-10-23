# Rust builder image

The [Rust Builder workflow](../../.github/workflows/rust-builder.yaml) builds and verifies native amd64 and arm64 images. After both architectures pass, master builds publish a combined manifest to the GHCR repository named in `repository`, tagged `sha-<full-commit>` and `latest`. Pull requests build and verify without publishing. A manual dispatch on master runs the same publishing pipeline.

The Dockerfile derives from a digest-pinned `start9/cargo-zigbuild` image. This preserves Rust 1.97.1, Zig 0.16.0, the macOS SDK, bindgen, musl cross-compilers, Alpine sysroots, and Rust target flags. The base remains a Docker Hub dependency; its digest is independent of later changes to that repository's tags.

A build stage downloads checksum-verified upstream cargo-zigbuild 0.24.0 source, applies `rva23.patch`, runs its library tests, and compiles the replacement executable. The patch preserves Start9's RVA23 default for `riscv64gc` C/C++ compilation using Zig's named profile. Explicit CPU overrides retain upstream behavior. The upstream release supplies the Apple dylib deduplication and deployment-target handling.

The final image contains the replacement executable and its verification script, not the downloaded source or intermediate Rust build artifacts.

## Local build

From the monorepo root:

```sh
make rust-builder
make rust-builder-check
```

`RUST_BUILDER_TEST_IMAGE` overrides the local image tag. The image check verifies macOS dylib deduplication on both architectures and both argument paths, RISC-V ISA defaults and CPU overrides through generated compiler wrappers, inherited toolchain assets, and a native Linux-musl Rust executable.

## Updates

Update the pinned source URL and checksum together. Re-evaluate the patch against that upstream release. Updating the base image digest is a separate toolchain/sysroot change.

GHCR associates the package with this repository through its OCI source label. Its package visibility must be public before unauthenticated developer builds can pull it.
