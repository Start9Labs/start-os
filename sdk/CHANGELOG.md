# Changelog

## 2.0.0 — StartOS 0.4.0-beta.9 (2026-05-20)

### Added

- `userspaceFilesystems` and `virtualNetworking` manifest flags split the former `nestedRuntime` flag into its two independent device grants. `userspaceFilesystems` mounts `/dev/fuse` for fuse-overlayfs storage (the rootless driver behind a nested OCI runtime). `virtualNetworking` mounts `/dev/net/tun` so a service can bring up kernel tun interfaces for VPN / WireGuard / tun-class workloads. The service LXC already retains `CAP_NET_ADMIN` within its user namespace via the standard `userns.conf` include, so no extra capability machinery is required — only the device node was missing
- `sdk.Daemons.dynamic(fn)` builds a reactive `main` entrypoint whose daemon set is a function of on-disk state. The supplied builder returns a regular `sdk.Daemons.of({ effects }).addDaemon(...)` chain (now record-then-materialize — see below), and the SDK diffs its entries against the running set on every `effects.constRetry` trigger (typically fired by a `FileHelper.read().const(effects)` watcher). Per id: absent → present **start**, present → absent **stop**, same `configHash` **leave alone**, different `configHash` **restart**. Dependents of any restarted or stopped daemon are also restarted to keep `requires` wiring consistent. Re-runs coalesce while one is in flight. Designed for multi-tenant packages like the registry-portal s9pk that add, rename, and delete sub-instance daemons without restarting the service
- `configHash` covers the subcontainer descriptor (`imageId`, `sharedRun`, `name`, structural `mounts.build()`), exec (`command`, `env`, `cwd`, `user`, `runAsInit`, `sigtermTimeout`), `requires` (sorted), and the structural parts of `ready` (`display`, `gracePeriod`). Closures (`ready.fn`, `ready.trigger`, function-form `exec.fn`) and pre-built `Daemon` instances are intentionally excluded — surface a value through one of the hashed fields if you want the reconciler to react to it changing
- `sdk.SubContainer.eager(...)` creates a SubContainer with its filesystem materialized immediately. Use when you need `createFs` failures to surface at the construction site instead of at first method call, or when you need sync access to `rootfs` / `guid` / `subpath()` before running any methods
- `SubContainerLazy.eager(): Promise<SubContainerEager<M>>` forces materialization on a lazy handle and returns the underlying eager subcontainer for callers that need the narrowed sync interface
- New compile-time validator `ValidateExVerRange<T>` validates exver version-**range** string literals across the full range grammar (comparison / caret / tilde anchors over `[#flavor:]upstream[:downstream]` specs, `&&` / `||`, parentheses, negation, `#flavor` atoms, and `*`), catching a malformed version atom anywhere in the expression. It now guards the two places a range literal is written: a dependency's `versionRange` (via `sdk.setupDependencies`) and the keys of `migrations.other`, so typos like `'>=2.f'` or `'^28 || 30.x:0'` fail at `tsc` time instead of at runtime. Purely additive types — runtime parsing is unchanged, and structural mistakes (unbalanced parens, a dangling operator) are still left to runtime validation

### Changed

- **Breaking — `sdk.SubContainer.of(...)` is now lazy by default.** Returns a `SubContainerLazy<M>` *synchronously* (no `Promise<>` wrapper) whose filesystem is materialized on first method call. `rootfs` / `guid` / `subpath()` on the unified `SubContainer<M>` interface widen to `T | Promise<T>`; the concrete classes narrow (`SubContainerEager` to `T`, `SubContainerLazy` to `Promise<T>`). Code holding the generic interface must `await` those accessors; code holding `SubContainerEager` keeps sync access. Migration: most callers already use only async methods (`.exec`, `.writeFile`, `.spawn`, `.launch`) and need no change beyond dropping the redundant `await` on `SubContainer.of(...)`; callers that read `.rootfs` / `.guid` / `.subpath()` add an `await` or switch to `sdk.SubContainer.eager(...)`. The lazy default enables `Daemons.dynamic`'s "leave alone is load-bearing" guarantee: unchanged daemons across reconciles never materialize a fresh subcontainer
- **Breaking — `SubContainerOwned` / `SubContainerRc` collapsed into `SubContainerEager`.** The reference-counted handle (`SubContainerRc`, `.rc()`, `.isOwned()`) is replaced by an internal hold-count on the unified `SubContainer`. Multiple consumers each call `sub.hold()` (returns a release fn); the container's `destroyFs` fires when `destroy()` has been called and the last hold is released, in either order. `Daemon.start()` takes its own hold for the daemon's lifetime and releases it on `term()`. The `destroySubcontainer` flag on `Daemon.term` / `HealthDaemon.term` is gone — `Daemons.term()` calls `destroy()` on each unique subcontainer in its entries, and the hold machinery handles sharing safely
- **Breaking — `Daemon.sharesSubcontainerWith` removed.** Two `Daemon`s share a subcontainer when constructed with the same `SubContainer` instance (compare `daemon.subcontainer.identity`). Daemons' internal "should I destroy this subc?" branching is gone — it always calls `destroy()`, and hold-count decides
- **Breaking — `Daemons` is record-then-materialize.** `.addDaemon()` / `.addOneshot()` / `.addHealthCheck()` append a recorded entry without constructing `HealthDaemon` or `Daemon`. `Daemons.build()` walks the entries and constructs the chain in one pass. Functionally identical for `setupMain` callers — the original "construct on addDaemon, kick off on build" timing is invisible because nothing actually starts until `build()`'s `updateStatus()` calls anyway. The change is load-bearing for `Daemons.dynamic`, which needs to diff entries without paying the cost of building them eagerly each reconcile
- **Breaking — `SubContainer` adds `identity: symbol`.** Sync, stable handle preserved across `SubContainerLazy.eager()` materialization. Use for sharing checks that must work before materialization (replaces `Daemon.sharesSubcontainerWith`'s previous `guid` comparison, which couldn't fire pre-materialization)
- Container-runtime updated: `SubContainerOwned` → `SubContainer.eager`; `SubContainerRc<M>` → `SubContainer<M>`; `daemon.subcontainerRc()` → `daemon.subcontainer`

### Removed

- **Breaking — `nestedRuntime` manifest flag removed**, with no compatibility alias. It conflated two unrelated device grants (`/dev/fuse` for fuse-overlayfs storage and `/dev/net/tun` for kernel tun interfaces). Replace with `userspaceFilesystems` (nested OCI runtimes) and/or `virtualNetworking` (kernel tun interfaces). Packages must republish: a host updating StartOS parses an old `nestedRuntime` field as absent, so both new flags default to `false`
- `SubContainerOwned`, `SubContainerRc`, `SubContainer.rc()`, `SubContainer.isOwned()` — folded into the unified `SubContainerEager` / `SubContainerLazy` with hold/release lifecycle
- `Daemon.subcontainerRc()`, `Daemon.markManaged()`, `Daemon.sharesSubcontainerWith()` — superseded by `daemon.subcontainer` (public readonly) and the hold-count model
- `destroySubcontainer` option on `Daemon.term` / `HealthDaemon.term` — `Daemons` calls `subcontainer.destroy()` for each unique subc on shutdown, and the hold-count decides actual timing

## 1.5.3 — StartOS 0.4.0-beta.9 (2026-05-20)

### Fixed

- `Backups.withMysqlDump` no longer wedges the entire backup (and the StartOS host) on the database-shutdown step. After the dump it stopped mysqld with `kill $PID && tail --pid=$PID -f /dev/null`, but the daemonized (mysql) / backgrounded (mariadb) mysqld is never reaped in the dump subcontainer, so it lingers as a zombie that keeps its PID and `tail --pid` waited on it forever. The subcontainer was then never torn down and the backup hung until the box was rebooted — often surfacing afterward as a misleading `ENOTCONN … mkdir` notification as mounts were unwound under the stuck operation. Shutdown now waits for mysqld to reach the zombie (`Z`) state or vanish (treating either as "exited"), with a bounded SIGKILL fallback so it can never deadlock again. Applies to both backup and restore. `withPgDump` was unaffected (it uses `pg_ctl stop -w`, which reaps cleanly). The `withMysqlDump` packages (ghost, mempool) should rebuild against this SDK

## 1.5.2 — StartOS 0.4.0-beta.9 (2026-05-15)

### Fixed

- `Backups.withPgDump` and `Backups.withMysqlDump` now stage the dump file in the subcontainer rootfs (`/tmp/<db>-db.dump`) and `cp` it through to the backup target, on both backup and restore. Previously `pg_dump` / `mysqldump` wrote directly to the `backup-fs` FUSE bind mount, where their writes were silently dropped — the process exited 0 but the resulting `<db>-db.dump` was 0 bytes. Every package using these helpers (immich, spliit, synapse, nextcloud, btcpayserver, mempool, ghost) has been shipping empty database dumps since the helpers were introduced; bump to this SDK and rebuild the s9pk to recover. Plain `cp` writes through the FUSE work, so staging through `/tmp` produces an identical on-disk dump file at the same path — no restore-format change

## 1.5.1 — StartOS 0.4.0-beta.9 (2026-05-13)

### Fixed

- `GetActionInputType` (used to infer the input shape for `TaskOptions` / `task()` partials) now matches against `ActionInfo` instead of `Action`. The conditional previously tested `A extends Action<...>` while `A` was constrained to `ActionInfo<...>`, so inference fell through to `never` and `TaskInput.value` collapsed to `DeepPartial<never>`. Tasks built from `ActionInfo` now infer their input type correctly
- `BindOptions.addSsl` is now `Partial<AddSslOptions>` for protocols without SSL variants, matching the type already used on the SSL-variant branch. The two branches of the discriminated union are now consistent, so callers can omit individual SSL option fields regardless of which protocol they're binding

## 1.5.0 — StartOS 0.4.0-beta.9 (2026-05-08)

### Added

- `sdk.notification.create(effects, options)` lets a package post a notification into the StartOS notifications panel, alongside the ones StartOS itself generates (e.g. on backup completion). `options` is `{ level, title, message, data? }`: omit `data` for a plain notification (panel row only) or pass markdown text for `data` to attach a long-form body the UI renders in a "View Details" modal (release notes, post-update changelogs, structured error reports). The host forces `packageId` to the calling service's id, so a package can't spoof another package
- Backed by a new effect `effects.notification.create(...)` (requires StartOS 0.4.0-beta.9 with the matching backend handler)

### Changed

- Minimum StartOS version bumped to `0.4.0-beta.9` — required for the `notification.create` effect added in this release

## 1.4.3 — StartOS 0.4.0-beta.8 (2026-05-07)

### Fixed

- `FileHelper.produce` no longer leaks an `abort` listener on its parent `AbortSignal` per `fs.watch` event. The listener was registered inside the `while` loop and never detached, so after ~11 file changes during a single `.const()` lifetime Node emitted `MaxListenersExceededWarning: Possible EventTarget memory leak detected. 11 abort listeners added to [AbortSignal]`. The listener is now scoped to a single iteration via `try`/`finally` so it is removed before the next iteration registers its own. Fixes [#3182](https://github.com/Start9Labs/start-os/issues/3182)

## 1.4.2 — StartOS 0.4.0-beta.8 (2026-05-06)

### Changed

- `VersionGraph.uninit` now throws when handed a range target with no satisfying version, instead of silently returning without advancing the data version. The previous silent behavior masked host bugs that produced contradictory targets (e.g. an `AND` of a flavored on-disk range with a flavor-less destination's `canMigrateFrom`) — uninit would skip the registered cross-flavor migration, leave the on-disk range untouched, and the destination's init would dead-end with `cannot migrate from <range> to <concrete>` with no signal at the source. Throwing surfaces the contract violation at the source where it can be diagnosed. Packages whose host correctly computes uninit targets are unaffected

## 1.4.1 — StartOS 0.4.0-beta.8 (2026-05-04)

### Fixed

- Pin `zod` to exact version `4.3.6`. Zod 4.4 changed `.catch()` to no longer fire on missing object keys (only on present-but-invalid values), which silently breaks every package's `.merge({})` seed pattern — required fields with `.catch()` defaults throw `"expected nonoptional, received undefined"` instead of falling back to the default. The previous `^4.3.6` range allowed packages installed after 4.4.0 dropped to pick up the new behavior on `npm install`. Pinning ensures consistent behavior across all packages regardless of when they were installed

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
