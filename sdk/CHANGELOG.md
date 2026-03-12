# Changelog

## 0.4.0-beta.59 — StartOS v0.4.0-alpha.20 (2026-03-06)

### Added

- Support for preferred external ports besides 443
- Bridge filter kind on service interfaces

### Fixed

- Merge version ranges when adding existing package signer
- Task fix for action task system

## 0.4.0-beta.56 — StartOS v0.4.0-alpha.19 (2026-02-02)

### Added

- `getOutboundGateway` effect and SDK wrapper
- Improved service version migration and data version handling
- `zod-deep-partial` integration with `partialValidator` on `InputSpec`
- SMTP rework with improved provider variants and system SMTP spec

### Changed

- Migrated from `ts-matches` to `zod` across all TypeScript packages
- Builder-style `InputSpec` API with prefill plumbing
- Split `row_actions` into `remove_action` and `overflow_actions` for URL plugins

### Fixed

- Scoped public domain to single binding and return single port check
- Preserved `z` namespace types for SDK consumers
- `--arch` flag falls back to emulation when native image unavailable

## 0.4.0-beta.54 — StartOS v0.4.0-alpha.18 (2026-01-27)

### Added

- Device info RPC
- Hardware acceleration and NVIDIA card support on nonfree images

### Changed

- Consolidated setup flow
- Improved SDK abort handling and `InputSpec` filtering

## 0.4.0-beta.49 — StartOS v0.4.0-alpha.17 (2026-01-10)

### Added

- JSDoc comments on all consumer-facing APIs
- StartTunnel random subnet support
- Port 80 to 5443 tunnel mapping

### Fixed

- `EffectCreator` type corrections
- Allow multiple equal signs in ENV `FileHelper` values
- Miscellaneous alpha.16 follow-up fixes

## 0.4.0-beta.45 — StartOS v0.4.0-alpha.16 (2025-12-18)

### Added

- `map` and `eq` on `getServiceInterface` watcher
- Flavor-aware version range handling

### Changed

- Refactored `StatusInfo` types
- Improved shutdown ordering for daemons
- Improved StartTunnel validation and garbage collection

## 0.4.0-beta.43 — StartOS v0.4.0-alpha.15 (2025-11-26)

### Fixed

- Minor bugfixes for alpha.14

## 0.4.0-beta.42 — StartOS v0.4.0-alpha.14 (2025-11-20)

### Fixed

- Bugfixes for alpha.13

## 0.4.0-beta.41 — StartOS v0.4.0-alpha.13 (2025-11-15)

### Fixed

- Bugfixes for alpha.12

## 0.4.0-beta.40 — StartOS v0.4.0-alpha.12 (2025-11-07)

### Added

- StartTunnel integration
- Configurable `textarea` rows in `InputSpec`

## 0.4.0-beta.39 — StartOS v0.4.0-alpha.11 (2025-09-24)

### Added

- Gateway limiting for StartTunnel
- Improved copy UX around Tor SSL

### Changed

- SDK type updates and internal improvements
