import * as T from '../../../base/lib/types'
import { once } from '../../../base/lib/util/once'
import { logErrorOnce } from '../../../base/lib/util/logErrorOnce'
import { asError } from '../../../base/lib/util/asError'
import { Mounts } from './Mounts'
import { ExecCommandOptions, Ready } from './Daemons'
import { Daemon } from './Daemon'
import { Oneshot } from './Oneshot'
import { HealthDaemon, EXIT_SUCCESS } from './HealthDaemon'
import { SubContainer, SubContainerOwned } from '../util/SubContainer'

/**
 * Describes a subcontainer to be created on demand by {@link Daemons.dynamic}.
 *
 * The SDK constructs the subcontainer when starting the daemon and destroys
 * it when stopping the daemon (or when the daemon's `configHash` changes and
 * it needs to be restarted). The descriptor is part of the daemon's
 * structural fingerprint — changing `imageId`, `sharedRun`, `name`, or
 * `mounts` causes the reconciler to restart the daemon on the next re-run.
 *
 * Use this with the spec-form {@link DaemonsPlan.addDaemon} /
 * {@link DaemonsPlan.addOneshot} rather than constructing a `SubContainer`
 * yourself.
 */
export type SubContainerSpec<M extends T.SDKManifest> = {
  /** Image to launch the subcontainer from. Must be declared in the manifest. */
  imageId: keyof M['images'] & T.ImageId
  /** When true, the host's `/run` is bind-mounted into the subcontainer. */
  sharedRun?: boolean
  /** Debug/identification name for the subcontainer (does not affect identity). */
  name: string
  /** Filesystem mounts to apply to the subcontainer at start. */
  mounts?: Mounts<M, never>
}

/** A `Daemon`-shaped entry in a {@link DaemonsPlan}. */
export type DaemonPlanEntry<M extends T.SDKManifest> = {
  kind: 'daemon'
  id: string
  subcontainerSpec: SubContainerSpec<M>
  exec: ExecCommandOptions
  ready: Ready
  requires: ReadonlyArray<string>
}

/** A `Oneshot`-shaped entry in a {@link DaemonsPlan}. */
export type OneshotPlanEntry<M extends T.SDKManifest> = {
  kind: 'oneshot'
  id: string
  subcontainerSpec: SubContainerSpec<M>
  exec: ExecCommandOptions
  requires: ReadonlyArray<string>
}

/** A standalone health-check entry in a {@link DaemonsPlan}. */
export type HealthPlanEntry = {
  kind: 'health'
  id: string
  ready: Ready
  requires: ReadonlyArray<string>
}

/** Union of all entry kinds the reconciler understands. */
export type PlanEntry<M extends T.SDKManifest> =
  | DaemonPlanEntry<M>
  | OneshotPlanEntry<M>
  | HealthPlanEntry

type ErrorDuplicateId<Id extends string> = `The id '${Id}' is already used`

/** Options accepted by {@link DaemonsPlan.addDaemon}. */
export type AddDaemonPlanParams<
  M extends T.SDKManifest,
  Ids extends string,
  Id extends string,
> = {
  /** Descriptor for the subcontainer the SDK will create at start time. */
  subcontainerSpec: SubContainerSpec<M>
  /** Command, env, and exec options applied when launching the daemon. */
  exec: ExecCommandOptions
  /** Health probe describing how to determine readiness. */
  ready: Ready
  /** Ids of prior entries that must be `success` before this daemon starts. */
  requires: Exclude<Ids, Id>[]
}

/** Options accepted by {@link DaemonsPlan.addOneshot}. */
export type AddOneshotPlanParams<
  M extends T.SDKManifest,
  Ids extends string,
  Id extends string,
> = {
  /** Descriptor for the subcontainer the SDK will create at start time. */
  subcontainerSpec: SubContainerSpec<M>
  /** Command, env, and exec options applied when launching the oneshot. */
  exec: ExecCommandOptions
  /** Ids of prior entries that must be `success` before this oneshot runs. */
  requires: Exclude<Ids, Id>[]
}

/** Options accepted by {@link DaemonsPlan.addHealthCheck}. */
export type AddHealthCheckPlanParams<Ids extends string, Id extends string> = {
  /** Health probe describing how to determine readiness. */
  ready: Ready
  /** Ids of prior entries that must be `success` before this check polls. */
  requires: Exclude<Ids, Id>[]
}

/**
 * Immutable, declarative description of a daemon topology.
 *
 * `DaemonsPlan` is the spec-form counterpart to {@link Daemons}. Where
 * `Daemons` builds health daemons and subcontainers eagerly as you chain
 * `.addDaemon(...)` calls, `DaemonsPlan` only *records* entries. The
 * resulting plan is then realised by {@link Daemons.dynamic}, which compares
 * it against the currently-running set on each re-run and starts, stops, or
 * leaves daemons alone based on structural diff.
 *
 * Entries are appended in declaration order; `requires` references must
 * point to earlier ids (enforced by the type system).
 */
export class DaemonsPlan<
  M extends T.SDKManifest,
  Ids extends string = never,
> {
  private constructor(readonly entries: ReadonlyArray<PlanEntry<M>>) {}

  /** Start a new empty plan. Chain `.addDaemon` / `.addOneshot` / `.addHealthCheck` to build it up. */
  static of<M extends T.SDKManifest>(): DaemonsPlan<M, never> {
    return new DaemonsPlan<M, never>([])
  }

  /** Append a long-running daemon entry. */
  addDaemon<Id extends string>(
    // prettier-ignore
    id:
      '' extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    opts: AddDaemonPlanParams<M, Ids, Id>,
  ): DaemonsPlan<M, Ids | Id> {
    const entry: DaemonPlanEntry<M> = {
      kind: 'daemon',
      id: id as string,
      subcontainerSpec: opts.subcontainerSpec,
      exec: opts.exec,
      ready: opts.ready,
      requires: opts.requires as string[],
    }
    return new DaemonsPlan<M, Ids | Id>([...this.entries, entry])
  }

  /** Append a one-shot command entry. */
  addOneshot<Id extends string>(
    // prettier-ignore
    id:
      '' extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    opts: AddOneshotPlanParams<M, Ids, Id>,
  ): DaemonsPlan<M, Ids | Id> {
    const entry: OneshotPlanEntry<M> = {
      kind: 'oneshot',
      id: id as string,
      subcontainerSpec: opts.subcontainerSpec,
      exec: opts.exec,
      requires: opts.requires as string[],
    }
    return new DaemonsPlan<M, Ids | Id>([...this.entries, entry])
  }

  /** Append a standalone health-check entry (no associated process). */
  addHealthCheck<Id extends string>(
    // prettier-ignore
    id:
      '' extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    opts: AddHealthCheckPlanParams<Ids, Id>,
  ): DaemonsPlan<M, Ids | Id> {
    const entry: HealthPlanEntry = {
      kind: 'health',
      id: id as string,
      ready: opts.ready,
      requires: opts.requires as string[],
    }
    return new DaemonsPlan<M, Ids | Id>([...this.entries, entry])
  }
}

/** The user-supplied builder consumed by {@link Daemons.dynamic}. */
export type DaemonsPlanBuilder<M extends T.SDKManifest> = (opts: {
  effects: T.Effects
}) => Promise<DaemonsPlan<M, any>> | DaemonsPlan<M, any>

/**
 * Stable JSON hash over the structural args of a plan entry.
 *
 * Two entries with the same id and the same `configHash` are considered "the
 * same daemon" by the reconciler — it leaves them running across re-runs.
 * Any difference triggers a restart.
 *
 * Hashed fields:
 * - `kind`, `id`, sorted `requires`
 * - subcontainer descriptor: `imageId`, `sharedRun`, `name`, and the
 *   structural `mounts.build()` output
 * - exec: `command`, `env`, `cwd`, `user`, `runAsInit`, `sigtermTimeout`
 * - ready: `display`, `gracePeriod`
 *
 * NOT hashed: `ready.fn`, `ready.trigger`, and any function-valued exec
 * options. Authors who want the reconciler to react to a value change must
 * surface that value through one of the hashed fields (e.g. embed it in
 * `exec.env` or `exec.command`).
 */
export function configHash<M extends T.SDKManifest>(
  entry: PlanEntry<M>,
): string {
  if (entry.kind === 'health') {
    return stableStringify({
      kind: 'health',
      id: entry.id,
      requires: [...entry.requires].sort(),
      display: entry.ready.display,
      gracePeriod: entry.ready.gracePeriod ?? null,
    })
  }
  const mounts = entry.subcontainerSpec.mounts?.build() ?? []
  const exec = entry.exec
  return stableStringify({
    kind: entry.kind,
    id: entry.id,
    requires: [...entry.requires].sort(),
    sub: {
      imageId: entry.subcontainerSpec.imageId,
      sharedRun: entry.subcontainerSpec.sharedRun ?? false,
      name: entry.subcontainerSpec.name,
      mounts,
    },
    exec: {
      command: normalizeCommand(exec.command),
      env: exec.env ?? null,
      cwd: exec.cwd ?? null,
      user: exec.user ?? null,
      runAsInit: exec.runAsInit ?? false,
      sigtermTimeout: exec.sigtermTimeout ?? null,
    },
    ready:
      entry.kind === 'daemon'
        ? {
            display: entry.ready.display,
            gracePeriod: entry.ready.gracePeriod ?? null,
          }
        : null,
  })
}

function normalizeCommand(c: T.CommandType): unknown {
  if (typeof c === 'string') return { kind: 'string', value: c }
  if (Array.isArray(c)) return { kind: 'argv', value: [...c] }
  if (T.isUseEntrypoint(c)) {
    return { kind: 'entrypoint', value: c.overridCmd ?? null }
  }
  return null
}

function stableStringify(v: unknown): string {
  return JSON.stringify(canonicalize(v))
}

function canonicalize(v: unknown): unknown {
  if (v === undefined || v === null) return null
  if (Array.isArray(v)) return v.map(canonicalize)
  if (typeof v === 'object') {
    const keys = Object.keys(v as object).sort()
    const out: Record<string, unknown> = {}
    for (const k of keys) out[k] = canonicalize((v as Record<string, unknown>)[k])
    return out
  }
  if (typeof v === 'function') return null
  return v
}

type RunningEntry<M extends T.SDKManifest> = {
  healthDaemon: HealthDaemon<M>
  subcontainer: SubContainerOwned<M> | null
  configHash: string
  requires: ReadonlyArray<string>
}

/**
 * Reconciles a {@link DaemonsPlan} against a running set of daemons.
 *
 * Construction is internal — get one by calling {@link Daemons.dynamic}.
 * Implements {@link T.DaemonBuildable}: `build()` performs the first
 * reconcile and wires up the re-run trigger via `effects.constRetry`;
 * `term()` shuts everything down in dependent-first order.
 */
export class DaemonsReconciler<M extends T.SDKManifest>
  implements T.DaemonBuildable
{
  private running = new Map<string, RunningEntry<M>>()
  private inProgress = false
  private pendingRerun = false
  private isTerminating = false
  private termPromise: Promise<void> | null = null
  private gen = 0

  constructor(
    private readonly rootEffects: T.Effects,
    private readonly fn: DaemonsPlanBuilder<M>,
  ) {}

  async build(): Promise<{ term(): Promise<void> }> {
    await this.runReconcile()
    this.rootEffects.onLeaveContext(() => {
      this.term().catch((e) => logErrorOnce(asError(e)))
    })
    return { term: () => this.term() }
  }

  /** Trigger an out-of-band reconcile. Internal — exposed for testing. */
  scheduleRerun(): void {
    if (this.isTerminating) return
    if (this.inProgress) {
      this.pendingRerun = true
      return
    }
    this.runReconcile().catch((e) => logErrorOnce(asError(e)))
  }

  private async runReconcile(): Promise<void> {
    if (this.isTerminating) return
    this.inProgress = true
    try {
      this.gen++
      const fnEffects = this.rootEffects.child(`dyn-daemons-fn`)
      fnEffects.constRetry = once(() => this.scheduleRerun())
      const plan = await this.fn({ effects: fnEffects })
      validatePlan(plan)
      await this.reconcile(plan)
    } catch (e) {
      logErrorOnce(asError(e))
    } finally {
      this.inProgress = false
      if (this.pendingRerun && !this.isTerminating) {
        this.pendingRerun = false
        // re-arm; coalesced rerun
        this.runReconcile().catch((e) => logErrorOnce(asError(e)))
      }
    }
  }

  private async reconcile(plan: DaemonsPlan<M, any>): Promise<void> {
    const desiredById = new Map<string, PlanEntry<M>>(
      plan.entries.map((e) => [e.id, e]),
    )
    const desiredHashes = new Map<string, string>(
      plan.entries.map((e) => [e.id, configHash(e)]),
    )

    const toStop = new Set<string>()
    for (const [id, current] of this.running) {
      const desired = desiredById.get(id)
      if (!desired) {
        toStop.add(id)
      } else if (desiredHashes.get(id) !== current.configHash) {
        toStop.add(id)
      }
    }

    // Transitively mark dependents of stopping entries
    let changed = true
    while (changed) {
      changed = false
      for (const [id, current] of this.running) {
        if (toStop.has(id)) continue
        if (current.requires.some((r) => toStop.has(r))) {
          toStop.add(id)
          changed = true
        }
      }
    }

    await this.stopEntries(toStop)

    // Start anything desired-but-not-running. plan.entries is in topo order
    // because requires can only reference earlier ids (enforced at the type
    // level by DaemonsPlan), so iterating in declaration order is safe.
    for (const entry of plan.entries) {
      if (this.running.has(entry.id)) continue
      try {
        await this.startEntry(entry, desiredHashes.get(entry.id)!)
      } catch (e) {
        logErrorOnce(asError(e))
      }
    }
  }

  private async stopEntries(ids: Set<string>): Promise<void> {
    if (ids.size === 0) return
    const order: string[] = []
    const visited = new Set<string>()
    const visit = (id: string) => {
      if (visited.has(id) || !ids.has(id)) return
      visited.add(id)
      for (const [otherId, other] of this.running) {
        if (other.requires.includes(id)) visit(otherId)
      }
      order.push(id)
    }
    for (const id of ids) visit(id)
    for (const id of order) {
      const entry = this.running.get(id)
      if (!entry) continue
      try {
        await entry.healthDaemon.term({ destroySubcontainer: false })
      } catch (e) {
        logErrorOnce(asError(e))
      }
      if (entry.subcontainer) {
        try {
          await entry.subcontainer.destroy()
        } catch (e) {
          logErrorOnce(asError(e))
        }
      }
      this.running.delete(id)
    }
  }

  private async startEntry(
    entry: PlanEntry<M>,
    hash: string,
  ): Promise<void> {
    const id = entry.id
    const daemonEffects = this.rootEffects.child(`daemon-${id}`)

    let subcontainer: SubContainerOwned<M> | null = null
    let daemon: Daemon<M> | null = null

    if (entry.kind !== 'health') {
      subcontainer = await SubContainerOwned.of<M, T.Effects>(
        daemonEffects,
        {
          imageId: entry.subcontainerSpec.imageId,
          sharedRun: entry.subcontainerSpec.sharedRun,
        },
        (entry.subcontainerSpec.mounts ?? null) as any,
        entry.subcontainerSpec.name,
      )
      try {
        const rc = subcontainer.rc() as SubContainer<M>
        if (entry.kind === 'daemon') {
          daemon = Daemon.of<M>()(daemonEffects, rc, entry.exec)
        } else {
          daemon = Oneshot.of<M>()(daemonEffects, rc, entry.exec)
        }
        daemon.markManaged()
      } catch (e) {
        // unwind subcontainer if daemon construction failed
        try {
          await subcontainer.destroy()
        } catch (destroyErr) {
          logErrorOnce(asError(destroyErr))
        }
        throw e
      }
    }

    const dependencies = entry.requires
      .map((reqId) => this.running.get(reqId)?.healthDaemon)
      .filter((d): d is HealthDaemon<M> => !!d)

    const readyArg: Ready | typeof EXIT_SUCCESS =
      entry.kind === 'oneshot' ? EXIT_SUCCESS : entry.ready

    const healthDaemon = new HealthDaemon<M>(
      daemon,
      dependencies,
      id,
      readyArg,
      this.rootEffects,
    )

    this.running.set(id, {
      healthDaemon,
      subcontainer,
      configHash: hash,
      requires: entry.requires,
    })

    await healthDaemon.updateStatus()
  }

  private async term(): Promise<void> {
    if (this.termPromise) return this.termPromise
    this.isTerminating = true
    this.termPromise = (async () => {
      const ids = new Set(this.running.keys())
      await this.stopEntries(ids)
    })()
    return this.termPromise
  }
}

function validatePlan<M extends T.SDKManifest>(
  plan: DaemonsPlan<M, any>,
): void {
  const seen = new Set<string>()
  for (const entry of plan.entries) {
    if (!entry.id) throw new Error(`DaemonsPlan: entry has empty id`)
    if (seen.has(entry.id)) {
      throw new Error(`DaemonsPlan: duplicate id '${entry.id}'`)
    }
    seen.add(entry.id)
    for (const req of entry.requires) {
      if (!seen.has(req)) {
        throw new Error(
          `DaemonsPlan: entry '${entry.id}' requires '${req}', which must be declared earlier`,
        )
      }
    }
  }
}

/**
 * Build a reactive `main` entrypoint that reconciles its daemon set against
 * a {@link DaemonsPlan} every time the plan changes.
 *
 * The provided builder is invoked once on startup and again on each
 * `effects.constRetry` trigger (e.g. when a watched file changes). The
 * reconciler diffs the new plan against the running set and:
 *
 * | Prior          | Next           | Action          |
 * | -------------- | -------------- | --------------- |
 * | absent         | present        | **start**       |
 * | present        | absent         | **stop**        |
 * | same hash      | same hash      | **leave alone** |
 * | different hash | different hash | **restart**     |
 *
 * Dependents of any restarted/stopped daemon are also restarted to keep
 * dependency wiring consistent.
 */
export function dynamicDaemons<M extends T.SDKManifest>(
  fn: DaemonsPlanBuilder<M>,
): T.ExpectedExports.main {
  return async ({ effects }) => new DaemonsReconciler<M>(effects, fn)
}
