import { Signals } from '../../../base/lib/types'

import { HealthCheckResult } from '../health/checkFns'

import { Trigger } from '../trigger'
import * as T from '../../../base/lib/types'
import { SubContainer } from '../util/SubContainer'

import { promisify } from 'node:util'
import * as CP from 'node:child_process'

export { Daemon } from './Daemon'
export { CommandController } from './CommandController'
import { EXIT_SUCCESS, HealthDaemon } from './HealthDaemon'
import { Daemon } from './Daemon'
import { CommandController } from './CommandController'
import { Oneshot } from './Oneshot'

/** Promisified version of `child_process.exec` */
export const cpExec = promisify(CP.exec)
/** Promisified version of `child_process.execFile` */
export const cpExecFile = promisify(CP.execFile)
/**
 * Configuration for a daemon's health-check readiness probe.
 *
 * Every daemon and standalone health check requires a `Ready` configuration
 * that tells StartOS how to determine whether the daemon is healthy.
 *
 * The `fn` is called on a recurring interval controlled by `trigger` (defaults
 * to 1 s before the first non-pending result, then 30 s). During the initial
 * `gracePeriod` window, `failure` results are softened to `starting` so the
 * UI doesn't flash red while a daemon is still booting.
 *
 * ### Health check result states
 *
 * | Result      | Meaning                                              | UI treatment     |
 * |-------------|------------------------------------------------------|------------------|
 * | `success`   | Healthy and fully operational                        | Green / ready    |
 * | `loading`   | Operational but still catching up (e.g. syncing)     | Progress / amber |
 * | `disabled`  | Intentionally inactive (excluded by configuration)   | Grey / skipped   |
 * | `starting`  | Not yet ready, still initializing                    | Spinner          |
 * | `waiting`   | Blocked on an external dependency                    | Spinner          |
 * | `failure`   | Unhealthy — something is wrong                       | Red / error      |
 */
export type Ready = {
  /**
   * Human-readable label shown in the StartOS health-check UI.
   * Set to `null` to hide this check from the UI entirely.
   */
  display: string | null
  /**
   * The function called on each polling interval to determine the daemon's health.
   *
   * Return a {@link HealthCheckResult} with a `result` field (`success`, `loading`,
   * `failure`, etc.) and an optional `message` string shown in the UI.
   *
   * The SDK ships several built-in helpers on `sdk.healthCheck`:
   * - `checkPortListening` — checks whether a TCP/UDP port is bound
   * - `checkWebUrl` — fetches a URL and succeeds on any HTTP response
   * - `runHealthScript` — runs a command in a subcontainer and succeeds on exit 0
   *
   * @example
   * ```ts
   * fn: () =>
   *   sdk.healthCheck.checkPortListening(effects, 80, {
   *     successMessage: 'Web server is ready',
   *     errorMessage: 'Web server is not listening',
   *   })
   * ```
   */
  fn: () => Promise<HealthCheckResult> | HealthCheckResult
  /**
   * Duration in milliseconds during which `failure` results are reported
   * as `starting` instead, giving the daemon time to initialize without
   * showing errors in the UI.
   *
   * @default 10_000
   */
  gracePeriod?: number
  /**
   * Controls the polling interval for this health check.
   *
   * Use one of the built-in triggers from `sdk.trigger`:
   * - `cooldownTrigger(ms)` — fixed interval between checks
   * - `statusTrigger({ success, loading, failure, ... })` — per-status
   *   polling intervals
   *
   * If omitted, uses the default trigger: 1 s before the first non-pending
   * result, then 30 s afterward.
   */
  trigger?: Trigger
}

/**
 * Options for running a daemon as a shell command inside a subcontainer.
 */
export type ExecCommandOptions = {
  /** The command and arguments to execute (e.g. `['bitcoind', '-conf=/etc/bitcoin.conf']`) */
  command: T.CommandType
  /**
   * How long (ms) to wait for the process to exit after sending SIGTERM
   * before force-killing it.
   *
   * @default 30_000
   */
  sigtermTimeout?: number
  /** Run the command as PID 1 inside the container (init process) */
  runAsInit?: boolean
  /** Environment variables to set for the process */
  env?:
    | {
        [variable in string]?: string
      }
    | undefined
  /** Working directory for the process */
  cwd?: string | undefined
  /** Run the process as this user inside the container */
  user?: string | undefined
  /** Callback invoked with each chunk written to stdout */
  onStdout?: (chunk: Buffer | string | any) => void
  /** Callback invoked with each chunk written to stderr */
  onStderr?: (chunk: Buffer | string | any) => void
}

/**
 * Options for running a daemon via an async function that may optionally return
 * a command to execute in the subcontainer. The function receives an `AbortSignal`
 * for cooperative cancellation.
 */
export type ExecFnOptions<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null,
> = {
  fn: (
    subcontainer: C,
    abort: AbortSignal,
  ) => Promise<C extends null ? null : ExecCommandOptions | null>
  // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
  sigtermTimeout?: number
}

/**
 * The execution specification for a daemon: either an {@link ExecFnOptions} (async function)
 * or an {@link ExecCommandOptions} (shell command, only valid when a subcontainer is provided).
 */
export type DaemonCommandType<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null,
> = ExecFnOptions<Manifest, C> | (C extends null ? never : ExecCommandOptions)

type NewDaemonParams<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null,
> = {
  /** What to run as the daemon: either an async fn or a commandline command to run in the subcontainer */
  exec: DaemonCommandType<Manifest, C>
  /** The subcontainer in which the daemon runs */
  subcontainer: C
}

type OptionalParamSync<T> = T | (() => T | null)
type OptionalParamAsync<T> = () => Promise<T | null>
type OptionalParam<T> = OptionalParamSync<T> | OptionalParamAsync<T>

type AddDaemonParams<
  Manifest extends T.SDKManifest,
  Ids extends string,
  Id extends string,
  C extends SubContainer<Manifest> | null,
> = (
  | NewDaemonParams<Manifest, C>
  | {
      daemon: Daemon<Manifest>
    }
) & {
  ready: Ready
  /**
   * IDs of prior daemons/oneshots/health checks that must be ready before
   * this daemon starts. Enforces startup ordering in the daemon chain.
   */
  requires: Exclude<Ids, Id>[]
}

type AddOneshotParams<
  Manifest extends T.SDKManifest,
  Ids extends string,
  Id extends string,
  C extends SubContainer<Manifest> | null,
> = NewDaemonParams<Manifest, C> & {
  exec: DaemonCommandType<Manifest, C>
  /**
   * IDs of prior daemons/oneshots/health checks that must be ready before
   * this oneshot runs.
   */
  requires: Exclude<Ids, Id>[]
}

type AddHealthCheckParams<Ids extends string, Id extends string> = {
  ready: Ready
  /**
   * IDs of prior daemons/oneshots/health checks that must be ready before
   * this health check starts polling.
   */
  requires: Exclude<Ids, Id>[]
}

type ErrorDuplicateId<Id extends string> = `The id '${Id}' is already used`

export const runCommand = <Manifest extends T.SDKManifest>() =>
  CommandController.of<Manifest, SubContainer<Manifest>>()

/**
 * A class for defining and controlling the service daemons
```ts
Daemons.of({
  effects,
  started,
  interfaceReceipt, // Provide the interfaceReceipt to prove it was completed
  healthReceipts, // Provide the healthReceipts or [] to prove they were at least considered
}).addDaemon('webui', {
  command: 'hello-world', // The command to start the daemon
  ready: {
    display: 'Web Interface',
    // The function to run to determine the health status of the daemon
    fn: () =>
      checkPortListening(effects, 80, {
        successMessage: 'The web interface is ready',
        errorMessage: 'The web interface is not ready',
      }),
  },
  requires: [],
})
```
 */
export class Daemons<Manifest extends T.SDKManifest, Ids extends string>
  implements T.DaemonBuildable
{
  private termPromise: Promise<void> | null = null
  private constructor(
    readonly effects: T.Effects,
    readonly ids: Ids[],
    readonly healthDaemons: HealthDaemon<Manifest>[],
  ) {}
  /**
   * Returns an empty new Daemons class with the provided inputSpec.
   *
   * Call .addDaemon() on the returned class to add a daemon.
   *
   * Daemons run in the order they are defined, with latter daemons being capable of
   * depending on prior daemons
   *
   * @param effects
   *
   * @param started
   * @returns
   */
  static of<Manifest extends T.SDKManifest>(options: { effects: T.Effects }) {
    return new Daemons<Manifest, never>(options.effects, [], [])
  }

  private addDaemonImpl<Id extends string>(
    id: Id,
    daemon: Daemon<Manifest, SubContainer<Manifest, T.Effects> | null> | null,
    requires: Ids[],
    ready: Ready | typeof EXIT_SUCCESS,
  ) {
    const healthDaemon = new HealthDaemon(
      daemon,
      requires
        .map((x) => this.ids.indexOf(x))
        .filter((x) => x >= 0)
        .map((id) => this.healthDaemons[id]),
      id,
      ready,
      this.effects,
    )
    const ids = [...this.ids, id] as (Ids | Id)[]
    const healthDaemons = [...this.healthDaemons, healthDaemon]
    return new Daemons<Manifest, Ids | Id>(this.effects, ids, healthDaemons)
  }

  /**
   * Register a long-running daemon process.
   *
   * The daemon starts in its subcontainer and is monitored by its `ready`
   * health check. Other daemons and health checks can depend on it via
   * `requires`.
   *
   * Pass a static options object, a sync/async factory that returns options
   * (or `null` to conditionally skip the daemon), or an object with a
   * pre-built `daemon` instance.
   *
   * @param id - Unique string identifier for this daemon
   * @param options - Daemon configuration, or a factory returning it (return `null` to skip)
   */
  addDaemon<Id extends string, C extends SubContainer<Manifest> | null>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamSync<AddDaemonParams<Manifest, Ids, Id, C>>,
  ): Daemons<Manifest, Ids | Id>
  addDaemon<Id extends string, C extends SubContainer<Manifest> | null>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamAsync<AddDaemonParams<Manifest, Ids, Id, C>>,
  ): Promise<Daemons<Manifest, Ids | Id>>
  addDaemon<Id extends string, C extends SubContainer<Manifest> | null>(
    id: Id,
    options: OptionalParam<AddDaemonParams<Manifest, Ids, Id, C>>,
  ) {
    const prev = this
    const res = (options: AddDaemonParams<Manifest, Ids, Id, C> | null) => {
      if (!options) return prev
      const daemon =
        'daemon' in options
          ? options.daemon
          : Daemon.of<Manifest>()<C>(
              this.effects,
              options.subcontainer,
              options.exec,
            )
      return prev.addDaemonImpl(id, daemon, options.requires, options.ready)
    }
    if (options instanceof Function) {
      const opts = options()
      if (opts instanceof Promise) {
        return opts.then(res)
      }
      return res(opts)
    }
    return res(options)
  }

  /**
   * Register a one-shot command that runs to completion before dependents start.
   *
   * Common uses: `chown` for file ownership, database migrations, config
   * generation, wallet unlocking. The oneshot is considered "ready" as soon
   * as the command exits successfully (exit code 0).
   *
   * @param id - Unique string identifier for this oneshot
   * @param options - Oneshot configuration, or a factory returning it (return `null` to skip)
   */
  addOneshot<Id extends string, C extends SubContainer<Manifest> | null>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamSync<AddOneshotParams<Manifest, Ids, Id, C>>,
  ): Daemons<Manifest, Ids | Id>
  addOneshot<Id extends string, C extends SubContainer<Manifest> | null>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamAsync<AddOneshotParams<Manifest, Ids, Id, C>>,
  ): Promise<Daemons<Manifest, Ids | Id>>
  addOneshot<Id extends string, C extends SubContainer<Manifest> | null>(
    id: Id,
    options: OptionalParam<AddOneshotParams<Manifest, Ids, Id, C>>,
  ) {
    const prev = this
    const res = (options: AddOneshotParams<Manifest, Ids, Id, C> | null) => {
      if (!options) return prev
      const daemon = Oneshot.of<Manifest>()<C>(
        this.effects,
        options.subcontainer,
        options.exec,
      )
      return prev.addDaemonImpl(id, daemon, options.requires, EXIT_SUCCESS)
    }
    if (options instanceof Function) {
      const opts = options()
      if (opts instanceof Promise) {
        return opts.then(res)
      }
      return res(opts)
    }
    return res(options)
  }

  /**
   * Register a standalone health check with no associated process.
   *
   * Use this for ongoing conditions that don't map to a single daemon, such
   * as blockchain sync progress or network reachability. Dependent services
   * can reference standalone health check IDs in their dependency config.
   *
   * @param id - Unique string identifier for this health check
   * @param options - Health check configuration, or a factory returning it (return `null` to skip)
   */
  addHealthCheck<Id extends string>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamSync<AddHealthCheckParams<Ids, Id>>,
  ): Daemons<Manifest, Ids | Id>
  addHealthCheck<Id extends string>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: OptionalParamAsync<AddHealthCheckParams<Ids, Id>>,
  ): Promise<Daemons<Manifest, Ids | Id>>
  addHealthCheck<Id extends string>(
    id: Id,
    options: OptionalParam<AddHealthCheckParams<Ids, Id>>,
  ) {
    const prev = this
    const res = (options: AddHealthCheckParams<Ids, Id> | null) => {
      if (!options) return prev
      return prev.addDaemonImpl(id, null, options.requires, options.ready)
    }
    if (options instanceof Function) {
      const opts = options()
      if (opts instanceof Promise) {
        return opts.then(res)
      }
      return res(opts)
    }
    return res(options)
  }

  /**
   * Start all registered daemons and wait until every one passes its ready
   * check, then tear everything down.
   *
   * Used for bootstrapping via a temporary daemon chain — e.g. starting a
   * service to call its API (create admin users, register apps), then
   * shutting it down before the real daemon chain starts.
   *
   * @param timeout - Maximum time (ms) to wait for all daemons to become ready, or `null` for no limit
   * @throws If the timeout is reached before all daemons are ready
   */
  async runUntilSuccess(timeout: number | null) {
    let resolve = (_: void) => {}
    const res = new Promise<void>((res, rej) => {
      resolve = res
      if (timeout)
        setTimeout(() => {
          const notReady = this.healthDaemons
            .filter((d) => !d.isReady)
            .map((d) => d.id)
          rej(new Error(`Timed out waiting for ${notReady}`))
        }, timeout)
    })
    const daemon = Oneshot.of()(this.effects, null, {
      fn: async () => {
        resolve()
        return null
      },
    })
    const healthDaemon = new HealthDaemon<Manifest>(
      daemon,
      [...this.healthDaemons],
      '__RUN_UNTIL_SUCCESS',
      'EXIT_SUCCESS',
      this.effects,
    )
    const daemons = await new Daemons<Manifest, Ids>(this.effects, this.ids, [
      ...this.healthDaemons,
      healthDaemon,
    ]).build()
    try {
      await res
    } finally {
      await daemons.term()
    }
    return null
  }

  /**
   * Gracefully terminate all daemons in reverse dependency order.
   *
   * Daemons with no remaining dependents are shut down first, proceeding
   * until all daemons have been terminated. Falls back to a bulk shutdown
   * if a dependency cycle is detected.
   */
  async term() {
    if (!this.termPromise) {
      this.termPromise = this._term()
    }
    return this.termPromise
  }

  private async _term() {
    const remaining = new Set(this.healthDaemons)

    while (remaining.size > 0) {
      // Find daemons with no remaining dependents
      const canShutdown = [...remaining].filter(
        (daemon) =>
          ![...remaining].some((other) =>
            other.dependencies.some((dep) => dep.id === daemon.id),
          ),
      )

      if (canShutdown.length === 0) {
        // Dependency cycle that should not happen, just shutdown remaining daemons
        console.warn(
          'Dependency cycle detected, shutting down remaining daemons',
        )
        canShutdown.push(...[...remaining].reverse())
      }

      // remove from remaining set
      canShutdown.forEach((daemon) => remaining.delete(daemon))

      // Shutdown daemons with no remaining dependents concurrently
      await Promise.allSettled(
        canShutdown.map(async (daemon) => {
          try {
            console.debug(`Terminating daemon ${daemon.id}`)
            const destroySubcontainer = daemon.daemon
              ? ![...remaining].some((d) =>
                  d.daemon?.sharesSubcontainerWith(daemon.daemon!),
                )
              : false
            await daemon.term({ destroySubcontainer })
          } catch (e) {
            console.error(e)
          }
        }),
      )
    }
  }

  /**
   * Start all registered daemons and their health checks.
   * @returns This `Daemons` instance, now running
   */
  async build() {
    this.effects.onLeaveContext(() => {
      this.term().catch((e) => console.error(e))
    })
    for (const daemon of this.healthDaemons) {
      daemon.daemon?.markManaged()
      await daemon.updateStatus()
    }
    return this
  }
}
