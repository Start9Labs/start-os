# Changelog

All notable changes to StartWRT are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Adding an Outbound VPN no longer silently does nothing. On submit the dialog
  called `tuiMarkControlAsTouchedAndValidate`, which re-ran the WireGuard `.conf`
  async validator and left the form stuck `PENDING` (the in-flight validation is
  cancelled when the file input remounts during the pending phase), so the create
  request was never sent. Submit now completes directly when the form is already
  valid, and only marks fields touched — without re-validating — when it isn't.
- Web-only changes are now re-embedded into the `startwrt` binary on rebuild. The
  UI is baked in at compile time via `include_dir!`, which does not register the
  embedded files as cargo dependencies, so `ctrl`'s `build.rs` now emits a
  `cargo:rerun-if-changed` for the web `dist` directory. Previously a changed web
  bundle was silently ignored unless a `.rs` file also changed, shipping a stale
  UI.

### Changed

- Relocated into the `start-technologies` monorepo as the `start-wrt` product. The
  three backend crates (`startwrt-core`/`ctrl`, `uciedit`, `uciedit_macros`) are now
  members of the root Cargo workspace and build against the **shared** `start-core`
  crate (`shared-libs/crates/start-core`, pulled in aliased as `startos`), the
  vendored `rpc-toolkit`, and the vendored `imbl-value` — replacing the previously
  embedded `start-os` submodule and the git/crates.io copies of those deps.
- `openwrt` is now the monorepo's only git submodule; the embedded `start-os`
  submodule was removed.
- Build orchestration moved from the standalone product `Makefile` to
  `projects/start-wrt/build.mk` (included by the root `Makefile`): `make startwrt`,
  `make startwrt-image`, `make startwrt-update`.

### Notes

- The Angular web app remains a standalone workspace (its own `package.json`) in this
  stage; folding it into the root Angular workspace and adopting `@start9labs/shared`
  is planned for a follow-up.

## [0.1.0-beta.3]

StartWRT is Start's fork of OpenWrt — a router OS for home self-hosting built around
per-device Security Profiles, with profiles assigned by Ethernet port, WiFi password,
or inbound VPN. Features inbound/outbound WireGuard VPNs with VPN chaining, WiFi
schedules, dynamic DNS, and published-port forwarding. A Rust backend (`startwrt`
binary: RPC daemon + CLI over JSON-RPC 2.0, UCI as source of truth) serves an embedded
Angular + Taiga UI frontend, shipped as a flashable OpenWrt image for the SpaceMiT K1
(BananaPi-F3).
