# Dependencies

Cross-service dependencies allow your service to interact with other StartOS services. Use them when your service needs to:

- Enforce configuration on a dependency (e.g., enable a feature)
- Register with a dependency (e.g., appservice registration)
- Read a dependency's interface URL at runtime

## Declaring Dependencies

Dependencies are declared in `manifest/index.ts`. Each dependency requires either `metadata` or `s9pk` to provide display info (title and icon). Both approaches achieve the same result -- they are two ways of providing the metadata:

```typescript
dependencies: {
  // Provide metadata directly
  synapse: {
    description: 'Needed for Matrix homeserver',
    optional: false,
    metadata: {
      title: 'Synapse',
      icon: '../synapse-wrapper/icon.png',
    },
  },

  // Extract metadata from an s9pk file
  electrs: {
    description: 'Provides an index for address lookups',
    optional: true,
    s9pk: 'https://github.com/org/repo/releases/download/v1.0/electrs.s9pk',
  },

  // s9pk: null when no s9pk URL is available
  'other-service': {
    description: 'Optional integration',
    optional: true,
    s9pk: null,
  },
}
```

## What `setupDependencies` Returns

The object you return from `setupDependencies()` declares what state each dependency should be in for your service to be considered "fully operational." It drives the **warning UI** the user sees on the service detail page — if a listed dependency isn't installed, isn't running, or has a listed health check failing, StartOS shows them a warning indicator and links them to the offending service.

It does **not** gate your service's startup. Your service starts whenever the user starts it, regardless of dependency state. The fields:

- `kind: 'running'` — user should have this dependency running. `kind: 'exists'` — user only needs it installed.
- `versionRange` — semver range the dependency must satisfy.
- `healthChecks` — names of the dependency's daemons (their `ready` IDs) or standalone health checks (`addHealthCheck` IDs) that should be passing.

If your service genuinely cannot operate before a dependency reaches a particular state (a file exists, an RPC responds, a config is generated), handle that at runtime in `setupMain` — poll the dependency, retry, or surface your own error. Don't rely on the dependency declaration to block startup for you.

## Creating Cross-Service Tasks

Use `sdk.action.createTask()` in `dependencies.ts` to trigger an action on a dependency. The action must be exported from the dependency's package.

```typescript
import { i18n } from './i18n'
import { sdk } from './sdk'
import { someAction } from 'dependency-package/startos/actions/someAction'

export const setDependencies = sdk.setupDependencies(async ({ effects }) => {
  await sdk.action.createTask(effects, 'dependency-id', someAction, 'critical', {
    input: {
      kind: 'partial',
      value: { /* fields matching the action's input spec */ },
    },
    when: { condition: 'input-not-matches', once: false },
    reason: i18n('Human-readable reason shown to user'),
  })

  return {
    'dependency-id': {
      kind: 'running',
      versionRange: '>=1.0.0:0',
      healthChecks: ['dependency-id'],
    },
  }
})
```

### API Signature

```typescript
sdk.action.createTask(
  effects,
  packageId: string,         // dependency service ID
  action: ActionDefinition,  // imported from the dependency package
  severity: 'critical' | 'high' | 'medium' | 'low',
  options?: {
    input?: { kind: 'partial', value: Partial<InputSpec> },
    when?: { condition: 'input-not-matches', once: boolean },
    reason: string,
    replayId?: string,       // prevents duplicate task execution
  }
)
```

> [!NOTE]
> - Import the action object from the dependency's published package.
> - The dependency must be listed in your `package.json` (e.g., `"synapse-startos": "file:../synapse-wrapper"`).
> - `when: { condition: 'input-not-matches', once: false }` re-triggers until the action's input matches.
> - `replayId` prevents duplicate tasks across restarts.

## Reading Dependency Interfaces

Use `sdk.serviceInterface.get()` in `main.ts` to read a dependency's interface at runtime:

```typescript
const url = await sdk.serviceInterface
  .get(
    effects,
    { id: 'interface-id', packageId: 'dependency-id' },
    (i) => {
      const urls = i?.addressInfo?.format()
      if (!urls || urls.length === 0) return null
      return urls[0]
    },
  )
  .const()  // re-runs setupMain if the interface changes
```

Alternatively, services are reachable directly by hostname at `http://<package-id>.startos:<port>`:

```typescript
const url = 'http://bitcoind.startos:8332'
```

## Mounting Dependency Volumes

Mount a dependency's volume for direct file access in `main.ts`:

```typescript
const mounts = sdk.Mounts.of()
  .mountVolume({ volumeId: 'main', subpath: null, mountpoint: '/data', readonly: false })
  .mountDependency({
    dependencyId: 'bitcoind',
    volumeId: 'main',
    subpath: null,
    mountpoint: '/mnt/bitcoind',
    readonly: true,
  })
```

## Init Order

Dependencies are resolved during initialization in this order:

```
restoreInit -> versionGraph -> setInterfaces -> setDependencies -> actions -> setup
```

`setInterfaces` runs before `setDependencies`, so your service's interfaces are available when creating cross-service tasks.
