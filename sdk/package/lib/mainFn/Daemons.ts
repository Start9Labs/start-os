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
 * Determines how the system knows when a daemon is healthy and ready to serve.
 */
export type Ready = {
  /** A human-readable display name for the health check. If null, the health check itself will be from the UI */
  display: string | null
  /**
   * @description The function to determine the health status of the daemon
   * 
   *   The SDK provides some built-in health checks. To see them, type sdk.healthCheck.
   * 
   * @example
   * ```
    fn: () =>
      sdk.healthCheck.checkPortListening(effects, 80, {
        successMessage: 'service listening on port 80',
        errorMessage: 'service is unreachable',
      })
  * ```
  */
  fn: () => Promise<HealthCheckResult> | HealthCheckResult
  /**
   * A duration in milliseconds to treat a failing health check as "starting"
   *
   * defaults to 5000
   */
  gracePeriod?: number
  trigger?: Trigger
}

/**
 * Options for running a daemon as a shell command inside a subcontainer.
 * Includes the command to run, optional signal/timeout, environment, user, and stdio callbacks.
 */
export type ExecCommandOptions = {
  command: T.CommandType
  // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
  sigtermTimeout?: number
  runAsInit?: boolean
  env?:
    | {
        [variable in string]?: string
      }
    | undefined
  cwd?: string | undefined
  user?: string | undefined
  onStdout?: (chunk: Buffer | string | any) => void
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
  /** An array of IDs of prior daemons whose successful initializations are required before this daemon will initialize */
  requires: Exclude<Ids, Id>[]
}

type AddOneshotParams<
  Manifest extends T.SDKManifest,
  Ids extends string,
  Id extends string,
  C extends SubContainer<Manifest> | null,
> = NewDaemonParams<Manifest, C> & {
  exec: DaemonCommandType<Manifest, C>
  /** An array of IDs of prior daemons whose successful initializations are required before this daemon will initialize */
  requires: Exclude<Ids, Id>[]
}

type AddHealthCheckParams<Ids extends string, Id extends string> = {
  ready: Ready
  /** An array of IDs of prior daemons whose successful initializations are required before this daemon will initialize */
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
   * Returns the complete list of daemons, including the one defined here
   * @param id
   * @param options
   * @returns a new Daemons object
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
   * Returns the complete list of daemons, including a "oneshot" daemon one defined here
   * a oneshot daemon is a command that executes once when started, and is considered "running" once it exits successfully
   * @param id
   * @param options
   * @returns a new Daemons object
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
   * Returns the complete list of daemons, including a new HealthCheck defined here
   * @param id
   * @param options
   * @returns a new Daemons object
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
   * Runs the entire system until all daemons have returned `ready`.
   * @param id
   * @param options
   * @returns a new Daemons object
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
    for (const daemon of this.healthDaemons) {
      await daemon.updateStatus()
    }
    return this
  }
}
