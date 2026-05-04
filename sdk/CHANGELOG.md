# Changelog

## 1.4.0 — StartOS 0.4.0-beta.8 (2026-05-03)

### Added

- `FileHelper.yaml` accepts an optional `options` argument that is threaded into both `YAML.stringify` and `YAML.parse`. Enables custom YAML tags (`!include`, `!secret`, `!include_dir_*`, etc.) to round-trip through the file model. Pass `customTags` to register tag handlers; pair the handler's `resolve` (returns a JS value during parse) with `identify`/`stringify` (re-emits the tag during write) to keep tag information across read-modify-write cycles. Other yaml options (e.g. `indent`, `lineWidth`, `aliasDuplicateObjects`) are accepted on the same object

### Changed

- **Breaking:** `FileHelper.yaml`'s transformers overload now takes `options` at position 3 and `transformers` at position 4, matching the argument order of `FileHelper.ini` and `FileHelper.xml`. Call sites that passed `transformers` as the third positional argument must be updated. The simple form (`FileHelper.yaml(path, shape)`) is unchanged

## 1.3.4 — StartOS 0.4.0-beta.8 (2026-05-02)

### Added

- `ProxyAuth` and `BasicCredential` bindings re-exported from `interfaces/Host`. Pass `auth` on `AddSslOptions` to put a `Bearer` or `Basic` gate in front of an HTTPS bind; the OS reverse proxy validates `Authorization` before forwarding upstream and (for `Basic`) injects the matched username as `X-Forwarded-User`. Setting `auth` implies HTTP-aware proxying. Optional `realm` controls the `WWW-Authenticate` challenge string

### Changed

- Minimum StartOS version bumped to `0.4.0-beta.8` — required for the reverse-proxy auth gate added in this release

## 1.3.3 — StartOS 0.4.0-beta.6 (2026-04-24)

### Fixed

- `checkPortListening` reads `/proc/net/{tcp,tcp6,udp,udp6}` via `fs.promises.readFile` instead of `cat` through `child_process.exec`. The prior implementation failed with `stdout maxBuffer length exceeded` on hosts where `/proc/net/tcp` exceeded 1 MB, causing false negatives on busy boxes

## 1.3.1 — StartOS 0.4.0-beta.6 (2026-04-21)

### Changed

- Minimum StartOS version bumped to `0.4.0-beta.6`

## 1.3.0 — StartOS 0.4.0-beta.4 (2026-04-21)

### Added

- `nullToUndefined(obj)` utility and `NullToUndefined<T>` type — recursively convert `null` values to `undefined` through objects and arrays

### Fixed

- Backup `rsync` no longer passes `--no-inc-recursive`; the pre-scan it forced was timing out on large backups. Accurate progress percentages go away as a side effect — to surface progress in the future, count files up front and compute percentage from bytes/files seen
- `withPgDump` pre-backup `touch` and `chown` of the dump file now fail loud instead of silently swallowing errors. Previously, if the backup target couldn't be written or chowned (e.g. a filesystem that doesn't honor Unix ownership), `pg_dump` would hit a confusing `Permission denied` on open instead of surfacing the underlying cause

## 1.2.0 — StartOS 0.4.0-beta.4 (2026-04-17)

### Added

- `footnote: string | null` on all scalar value specs (`text`, `textarea`, `number`, `color`, `datetime`, `toggle`, `select`, `multiselect`) — supplementary text rendered persistently beneath the field. Useful for showing upstream defaults, units, or any dev-authored clarification
- `Value.triState(...)` / `Value.dynamicTriState(...)` — a three-state toggle (boolean toggle with a neutral middle position), rendered as three icon buttons (✕ / — / ✓). Left outputs `false`, right outputs `true`, middle outputs `null`

## 1.1.0 — StartOS 0.4.0-beta.4 (2026-04-15)

### Added

- `trigger.statusTrigger(defaultMs, overrides?)`: single trigger that maps each `HealthStatus` to a polling interval, replacing `changeOnFirstSuccess`, `successFailure`, and `lastStatus`
- TSDocs across the public daemon/health-check surface: `Ready`, `addDaemon`, `addOneshot`, `addHealthCheck`, `runUntilSuccess`, the `trigger` namespace, `healthCheck` helpers, and `HealthCheckResult`

### Changed

- Converted `VersionInfo`, `Action`, `Actions`, and `FileHelper` from classes to interfaces with companion objects, so services importing types across differing SDK versions no longer hit "type X does not match type X" nominal-typing errors

### Fixed

- Default daemon trigger now polls quickly for `starting`/`waiting`/`failure` and slowly for `success`/`loading`/`disabled`; previously `changeOnFirstSuccess` only transitioned on `success`, so a daemon stuck in `loading` (e.g. during Bitcoin IBD) was polled every 1s indefinitely

### Removed

- `trigger.changeOnFirstSuccess`, `trigger.successFailure`, `trigger.lastStatus` (use `statusTrigger` instead)

## 1.0.0 — StartOS 0.4.0-beta.0 (2026-03-31)

### Changed

- **Breaking:** Minimum StartOS version bumped to `0.4.0-beta.0` (from `0.4.0-alpha.23`) — marks the transition out of alpha

### Added

- `GenerateCertificateParams` / `GenerateCertificateResponse` bindings for certificates signed by the root CA
- `RestartReason` binding and `ServerStatus.restart` field for unified, reason-specific restart notifications
- `PackageVersionInfo.satisfies` field so dependency breakage checks can match flavored package versions against flavorless ranges

### Fixed

- `deepEqual` now handles non-plain objects by reference instead of walking them as structural values

## 0.4.0-beta.66 (2026-03-24)

- **Breaking:** `withPgDump()` replaces `pgdata` with required `mountpoint` + `pgdataPath`
- Passwordless/trust auth support for `withPgDump()` and `withMysqlDump()`
- New options: `pgOptions` for postgres, `mysqldOptions` for mysql/mariadb
- Fixed MariaDB backup/restore support

## 0.4.0-beta.65 (2026-03-23)

### Added

- `Backups.withPgDump()`: dump-based PostgreSQL backup using `pg_dump`/`pg_restore`, replacing raw volume rsync of PG data directories
- `Backups.withMysqlDump()`: dump-based MySQL/MariaDB backup using `mysqldump`/`mysql`
- Password configs accept `string | (() => string | Promise<string>)` for deferred resolution during restore

## 0.4.0-beta.63 — StartOS v0.4.0-alpha.22 (2026-03-22)

### Fixed

- Fixed `createTask` failing when input values are undefined
- Fixed daemon lifecycle cleanup and error logging improvements
- Replaced fire-and-forget restart loop in `Daemon` with tracked `AbortController`
- Fixed graceful shutdown for subcontainer daemons
- Fixed rsync progress regex never matching, spamming logs during backup
- Fixed rsync backup bugs and optimized flags for encrypted CIFS targets
- Fixed types in `inputSpecConstants`, `StartSdk`, and exports

## 0.4.0-beta.62 (2026-03-19)

### Fixed

- Fixed `Value.dynamicSelect` and `Value.dynamicMultiselect` crashing with `z.union([])` when `values` is empty (zod v4 compatibility)

### Added

- `FileHelper.xml`: file helper for XML files using `fast-xml-parser`
- `smtpShape`: typed zod schema for persisting SMTP selection in store file models, replacing direct use of `smtpInputSpec.validator` which caused cross-zod-instance errors

## 0.4.0-beta.61 — StartOS v0.4.0-alpha.21 (2026-03-16)

### Fixed

- Fixed bug where leaving the effect context triggered consts

## 0.4.0-beta.60 — StartOS v0.4.0-alpha.20 (2026-03-16)

### Added

- Tunnel TS type exports and port forward labels
- Secure Boot MOK key enrollment fields in `SetupInfo`

### Changed

- Consolidated `Watchable` base class with generic `map`/`eq` support; renamed `call` to `fetch`
- Moved `GetServiceManifest` and `GetSslCertificate` from `package/` to `base/`
- Simplified `getServiceInterface`, `getServiceInterfaces`, `GetOutboundGateway`, `GetSystemSmtp`, and `fileHelper` using `Watchable` base class
- Simplified SDK Makefile with rsync

### Fixed

- Added `restart_again` flag to `DesiredStatus::Restarting`

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
