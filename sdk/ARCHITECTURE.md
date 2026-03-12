# SDK Architecture

The Start SDK is split into two npm packages that form a layered architecture: **base** provides the foundational types, ABI contract, and effects interface; **package** builds on base to provide the developer-facing SDK facade.

```
┌─────────────────────────────────────────────────────────────┐
│  package/ (@start9labs/start-sdk)                           │
│  Developer-facing facade, daemon management, health checks, │
│  backup system, file helpers, triggers, subcontainers       │
│                                                             │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  base/ (@start9labs/start-sdk-base)                   │  │
│  │  ABI, Effects, OS bindings, actions/input builders,   │  │
│  │  ExVer parser, interfaces, dependencies, S9pk, utils  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
         │                                        ▲
         │ Effects calls (RPC)                    │ Callbacks
         ▼                                        │
┌─────────────────────────────────────────────────────────────┐
│  StartOS Runtime (Rust supervisor)                          │
│  Executes effects, manages containers, networking, storage  │
└─────────────────────────────────────────────────────────────┘
```

The SDK follows [Semantic Versioning](https://semver.org/) within the `0.4.0-beta.*` pre-release series. The SDK version tracks independently from the StartOS release versions.

## Base Package (`base/`)

The base package is a self-contained library of types, interfaces, and low-level builders. It has no dependency on the package layer and can be used independently when only type definitions or validation are needed.

### OS Bindings (`base/lib/osBindings/`)

~325 auto-generated TypeScript files defining every type exchanged between the SDK and the StartOS runtime. These cover the full surface area of the system: manifests, actions, health checks, service interfaces, bind parameters, dependency requirements, alerts, SSL, domains, SMTP, networking, images, and more.

All bindings are re-exported through `base/lib/osBindings/index.ts`.

Key types include:
- `Manifest` — The full service package manifest as understood by the OS
- `ActionMetadata` — Describes an action's name, description, visibility, and availability
- `BindParams` — Port binding configuration (protocol, hostId, internal port)
- `ServiceInterface` — A network endpoint exported to users
- `DependencyRequirement` — Version range and health check requirements for a dependency
- `SetHealth` — Health check result reporting
- `HostnameInfo` / `Host` — Hostname and host metadata

### ABI and Core Types (`base/lib/types.ts`)

Defines the Application Binary Interface — the contract every service package must fulfill:

```typescript
namespace ExpectedExports {
  main       // Start the service daemon(s)
  init       // Initialize on install/update/restore
  uninit     // Clean up on uninstall/update/shutdown
  manifest   // Service metadata
  actions    // User-invocable operations
  createBackup // Export service data
}
```

Also defines foundational types used throughout the SDK:
- `Daemon` / `DaemonReturned` — Running process handles with `wait()` and `term()`
- `CommandType` — Shell string, argv array, or `UseEntrypoint`
- `ServiceInterfaceType` — `'ui' | 'api' | 'p2p'`
- `SmtpValue` — SMTP server configuration
- `KnownError` — Structured user-facing errors
- `DependsOn` — Package-to-health-check dependency mapping
- `PathMaker`, `MaybePromise`, `DeepPartial`, `DeepReadonly` — Utility types

### Effects Interface (`base/lib/Effects.ts`)

The bridge between TypeScript service code and the StartOS runtime. Every runtime capability is accessed through an `Effects` object passed to lifecycle hooks.

Effects are organized by subsystem:

| Subsystem | Methods | Purpose |
|-----------|---------|---------|
| **Action** | `export`, `clear`, `getInput`, `run`, `createTask`, `clearTasks` | Register and invoke user actions |
| **Control** | `restart`, `shutdown`, `getStatus`, `setMainStatus` | Service lifecycle control |
| **Dependency** | `setDependencies`, `getDependencies`, `checkDependencies`, `mount`, `getInstalledPackages`, `getServiceManifest` | Inter-service dependency management |
| **Health** | `setHealth` | Report health check results |
| **Subcontainer** | `createFs`, `destroyFs` | Container filesystem management |
| **Networking** | `bind`, `getServicePortForward`, `clearBindings`, `getHostInfo`, `getContainerIp`, `getOsIp`, `getOutboundGateway` | Port binding and network info |
| **Interfaces** | `exportServiceInterface`, `getServiceInterface`, `listServiceInterfaces`, `clearServiceInterfaces` | Service endpoint management |
| **Plugin** | `plugin.url.register`, `plugin.url.exportUrl`, `plugin.url.clearUrls` | Plugin system hooks |
| **SSL** | `getSslCertificate`, `getSslKey` | TLS certificate management |
| **System** | `getSystemSmtp`, `setDataVersion`, `getDataVersion` | System-wide configuration |

Effects also support reactive callbacks: many methods accept an optional `callback` parameter that the runtime invokes when the underlying value changes, enabling the reactive subscription patterns (`const()`, `watch()`, etc.).

### Action and Input System (`base/lib/actions/`)

#### Actions (`setupActions.ts`)

The `Action` class defines user-invocable operations. Two factory methods:
- `Action.withInput(id, metadata, inputSpec, prefill, execute)` — Action with a validated form
- `Action.withoutInput(id, metadata, execute)` — Action without user input

`Actions` is a typed map accumulated via `.addAction()` chaining.

#### Input Specification (`actions/input/`)

A builder-pattern system for declaring validated form inputs:

```
inputSpec/
├── builder/
│   ├── inputSpec.ts   — InputSpec.of() entry point
│   ├── value.ts       — Value class (individual form fields)
│   ├── list.ts        — List builder (arrays of values)
│   └── variants.ts    — Variants/Union builder (conditional fields)
├── inputSpecTypes.ts  — Type definitions for all field types
└── inputSpecConstants.ts — Pre-built specs (SMTP, etc.)
```

Supported field types via `Value`:
- `text`, `textarea`, `number` — Text and numeric input
- `toggle` — Boolean switch
- `select`, `multiselect` — Single/multi-choice dropdown
- `list` — Repeatable array of sub-values
- `color`, `datetime` — Specialized pickers
- `object` — Nested sub-form
- `union` / `dynamicUnion` — Conditional fields based on a discriminator

### Dependencies (`base/lib/dependencies/`)

- `setupDependencies.ts` — Declare what the service depends on (package IDs, version ranges, health checks)
- `dependencies.ts` — Runtime dependency checking via `checkDependencies()`

### Interfaces (`base/lib/interfaces/`)

Network interface declaration and port binding:

- `setupInterfaces.ts` — Top-level `setupServiceInterfaces()` function
- `Host.ts` — `MultiHost` class for binding ports and exporting interfaces. A single MultiHost can bind a port and export multiple interfaces (e.g. a primary UI and admin UI on the same port with different paths)
- `ServiceInterfaceBuilder.ts` — Builder for constructing `ServiceInterface` objects with name, type, description, scheme overrides, username, path, and query params
- `setupExportedUrls.ts` — URL plugin support for exporting URLs to other services

### Initialization (`base/lib/inits/`)

- `setupInit.ts` — Compose init scripts that run on install, update, restore, or boot
- `setupUninit.ts` — Compose uninit scripts that run on uninstall, update, or shutdown
- `setupOnInit` / `setupOnUninit` — Register callbacks for specific init/uninit events

Init scripts receive a `kind` parameter (`'install' | 'update' | 'restore' | null`) so they can branch logic based on the initialization context.

### Extended Versioning (`base/lib/exver/`)

A PEG parser-based versioning system that extends semver:

- **`Version`** — Standard semantic version (`1.2.3-beta.1`)
- **`ExtendedVersion` (ExVer)** — Adds an optional flavor prefix and a downstream version: `#flavor:upstream:downstream`
- **`VersionRange`** — Boolean expressions over version comparisons (`>=1.0.0 && <2.0.0 || =3.0.0`)

The parser is generated from `exver.pegjs` via Peggy and emitted as `exver.ts`.

ExVer separates upstream project versions from StartOS wrapper versions, allowing the package maintainer's versioning to evolve independently from the upstream software.

### S9pk Format (`base/lib/s9pk/`)

Parser and verifier for `.s9pk` service package archives:

- `S9pk` class — Deserialize and inspect package contents
- Merkle archive support for cryptographic verification of package integrity
- Methods: `deserialize()`, `icon()`, `license()`, etc.

### Utilities (`base/lib/util/`)

~28 utility modules including:

**Reactive subscription wrappers** — Each wraps an Effects callback-based method into a consistent reactive API:
- `Watchable` — Base class providing `const()`, `once()`, `watch()`, `onChange()`, `waitFor()`
- `GetContainerIp`, `GetStatus`, `GetSystemSmtp`, `GetOutboundGateway`, `GetSslCertificate`, `GetHostInfo`, `GetServiceManifest` — Typed wrappers for specific Effects methods

**General utilities:**
- `deepEqual` / `deepMerge` — Deep object comparison and merging
- `patterns` — Hostname regex, port validators
- `splitCommand` — Parse shell command strings into argv arrays
- `Drop` — RAII-style cleanup utility
- `graph` — Dependency graph utilities

## Package Layer (`package/`)

The package layer provides the developer-facing API. It re-exports everything from base and adds higher-level abstractions.

### StartSdk Facade (`package/lib/StartSdk.ts`)

The primary entry point for service developers. Constructed via a builder chain:

```typescript
const sdk = StartSdk.of()
  .withManifest(manifest)
  .build(true)
```

The `.build()` method returns an object containing the entire SDK surface area, organized by concern:

| Category | Members | Purpose |
|----------|---------|---------|
| **Manifest** | `manifest`, `volumes` | Access manifest data and volume paths |
| **Actions** | `Action.withInput`, `Action.withoutInput`, `Actions`, `action.run`, `action.createTask`, `action.createOwnTask`, `action.clearTask` | Define and manage user actions |
| **Daemons** | `Daemons.of`, `Daemon.of`, `setupMain` | Configure service processes |
| **Health** | `healthCheck.checkPortListening`, `.checkWebUrl`, `.runHealthScript` | Built-in health checks |
| **Interfaces** | `createInterface`, `MultiHost.of`, `setupInterfaces`, `serviceInterface.*` | Network endpoint management |
| **Backups** | `setupBackups`, `Backups.ofVolumes`, `Backups.ofSyncs`, `Backups.withOptions` | Backup configuration |
| **Dependencies** | `setupDependencies`, `checkDependencies` | Dependency declaration and verification |
| **Init/Uninit** | `setupInit`, `setupUninit`, `setupOnInit`, `setupOnUninit` | Lifecycle hooks |
| **Containers** | `SubContainer.of`, `SubContainer.withTemp`, `Mounts.of` | Container execution with mounts |
| **Forms** | `InputSpec.of`, `Value`, `Variants`, `List` | Form input builders |
| **Triggers** | `trigger.defaultTrigger`, `.cooldownTrigger`, `.changeOnFirstSuccess`, `.successFailure` | Health check polling strategies |
| **Reactive** | `getContainerIp`, `getStatus`, `getSystemSmtp`, `getOutboundGateway`, `getSslCertificate`, `getServiceManifest` | Subscription-based data access |
| **Plugins** | `plugin.url.register`, `plugin.url.exportUrl` | Plugin system (gated by manifest `plugins` field) |
| **Effects** | `restart`, `shutdown`, `setHealth`, `mount`, `clearBindings`, ... | Direct effect wrappers |
| **Utilities** | `nullIfEmpty`, `useEntrypoint`, `patterns`, `setDataVersion`, `getDataVersion` | Misc helpers |

### Daemon Management (`package/lib/mainFn/`)

The daemon subsystem manages long-running processes:

```
mainFn/
├── Daemons.ts          — Multi-daemon topology builder
├── Daemon.ts           — Single daemon wrapper
├── HealthDaemon.ts     — Health check executor
├── CommandController.ts — Command execution controller
├── Mounts.ts           — Volume/asset/dependency mount builder
├── Oneshot.ts          — One-time startup commands
└── index.ts            — setupMain() entry point
```

**Daemons** is a builder that accumulates process definitions:
```typescript
sdk.Daemons.of(effects)
  .addDaemon('db', { /* command, ready probe, mounts */ })
  .addDaemon('app', { requires: ['db'], /* ... */ })
  .addHealthCheck('primary', { /* ... */ })
```

Features:
- Startup ordering via `requires` (dependency graph between daemons)
- Ready probes (wait for a daemon to be ready before starting dependents)
- Graceful shutdown with configurable signals and timeouts
- One-shot commands that run before daemons start

**Mounts** declares what to attach to a container:
```typescript
sdk.Mounts.of()
  .mountVolume('main', '/data')
  .mountAssets('scripts', '/scripts')
  .mountDependency('bitcoind', 'main', '/bitcoin-data', { readonly: true })
  .mountBackup('/backup')
```

### Health Checks (`package/lib/health/`)

```
health/
├── HealthCheck.ts       — Periodic probe with startup grace period
└── checkFns/
    ├── checkPortListening.ts — TCP port connectivity check
    ├── checkWebUrl.ts        — HTTP(S) status code check
    └── runHealthScript.ts    — Script exit code check
```

Health checks are paired with **triggers** that control polling behavior:
- `defaultTrigger` — Fixed interval (e.g. every 30s)
- `cooldownTrigger` — Wait longer after failures
- `changeOnFirstSuccess` — Rapid polling until first success, then slow down
- `successFailure` — Different intervals for healthy vs unhealthy states

### Backup System (`package/lib/backup/`)

```
backup/
├── setupBackups.ts — Top-level setup function
└── Backups.ts      — Volume selection and rsync options
```

Three builder patterns:
- `Backups.ofVolumes('main', 'data')` — Back up entire volumes
- `Backups.ofSyncs([{ dataPath, backupPath }])` — Custom sync pairs
- `Backups.withOptions({ exclude: ['cache/'] })` — Rsync options

### File Helpers (`package/lib/util/fileHelper.ts`)

Type-safe configuration file management:

```typescript
const configFile = FileHelper.yaml(effects, sdk.volumes.main.path('config.yml'), {
  port: 8080,
  debug: false,
})

// Reactive reading
const config = await configFile.read.const(effects)

// Partial merge
await configFile.merge({ debug: true })

// Full write
await configFile.write({ port: 9090, debug: true })
```

Supported formats: JSON, YAML, TOML, INI, ENV, and custom parsers.

### Subcontainers (`package/lib/util/SubContainer.ts`)

Execute commands in isolated container environments:

```typescript
// Long-lived subcontainer
const container = await sdk.SubContainer.of(effects, { imageId: 'main' }, mounts, 'app')

// One-shot execution
await sdk.SubContainer.withTemp(effects, { imageId: 'main' }, mounts, 'migrate', async (c) => {
  await c.exec(['run-migrations'])
})
```

### Manifest Building (`package/lib/manifest/`)

```typescript
const manifest = setupManifest({
  id: 'my-service',
  title: 'My Service',
  license: 'MIT',
  description: { short: '...', long: '...' },
  images: { main: { source: { dockerTag: 'myimage:1.0' } } },
  volumes: { main: {} },
  dependencies: {},
  // ...
})

export default buildManifest(manifest)
```

`buildManifest()` finalizes the manifest with the current SDK version, OS version compatibility, and migration version ranges.

### Versioning (`package/lib/version/`)

Helpers for data version management during migrations:

```typescript
await sdk.setDataVersion(effects, '1.2.0:0')
const version = await sdk.getDataVersion(effects)
```

Used in init scripts to track which migration version the service's data has been brought to.

### Internationalization (`package/lib/i18n/`)

```typescript
const t = setupI18n({ en_US: enStrings, es_ES: esStrings })
const greeting = t('hello', { name: 'World' }) // "Hello, World!" or "Hola, World!"
```

Supports locale fallback and Intl-based formatting.

### Triggers (`package/lib/trigger/`)

Polling strategy functions that determine when health checks run:

```typescript
sdk.trigger.defaultTrigger({ timeout: 30_000 })
sdk.trigger.cooldownTrigger({ timeout: 30_000, cooldown: 60_000 })
sdk.trigger.changeOnFirstSuccess({ first: 5_000, then: 30_000 })
sdk.trigger.successFailure({ success: 60_000, failure: 10_000 })
```

## Build Pipeline

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed build instructions, make targets, and development workflow.

At a high level: Peggy generates the ExVer parser, TypeScript compiles both packages in strict mode (base to `baseDist/`, package to `dist/`), hand-written `.js`/`.d.ts` pairs are copied into the output, and `node_modules` are bundled for self-contained distribution.

## Data Flow

A typical service package lifecycle:

```
1. INSTALL / UPDATE / RESTORE
   ├── init({ effects, kind })
   │   ├── Version migrations (if update)
   │   ├── setupDependencies()
   │   ├── setupInterfaces() → bind ports, export interfaces
   │   └── Actions registration → export actions to OS
   │
2. MAIN
   │   setupMain() → Daemons.of(effects)
   │   ├── Oneshots run first
   │   ├── Daemons start in dependency order
   │   ├── Health checks begin polling
   │   └── Service runs until shutdown/restart
   │
3. SHUTDOWN / UNINSTALL / UPDATE
   │   uninit({ effects, target })
   │   └── Version down-migrations (if needed)
   │
4. BACKUP (user-triggered)
       createBackup({ effects })
       └── rsync volumes to backup location
```

## Key Design Patterns

### Builder Pattern
Most SDK APIs use immutable builder chains: `Daemons.of().addDaemon().addHealthCheck()`, `Mounts.of().mountVolume().mountAssets()`, `Actions.of().addAction()`. This provides type accumulation — each chained call narrows the type to reflect what has been configured.

### Effects as Capability System
All runtime interactions go through the `Effects` object rather than direct system calls. This makes the runtime boundary explicit, enables the OS to mediate all side effects, and makes service code testable by providing mock effects.

### Reactive Subscriptions
The `Watchable` base class provides a consistent API for values that can change over time:
- `const(effects)` — Read once; if the value changes, triggers a retry of the enclosing context
- `once()` — Read once without reactivity
- `watch()` — Async generator yielding on each change
- `onChange(callback)` — Invoke callback on each change
- `waitFor(predicate)` — Block until a condition is met

### Type-safe Manifest Threading
The manifest type flows through the entire SDK via generics. When you call `StartSdk.of().withManifest(manifest)`, the manifest's volume names, image IDs, dependency IDs, and plugin list become available as type constraints throughout all subsequent API calls. For example, `Mounts.of().mountVolume()` only accepts volume names declared in the manifest.
