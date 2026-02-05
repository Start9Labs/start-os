/**
 * @module Daemons
 *
 * This module provides the Daemons class for managing service processes (daemons)
 * and their health checks. Daemons are long-running processes that make up the
 * core functionality of a StartOS service.
 *
 * @example
 * ```typescript
 * // Basic daemon setup
 * export const main = sdk.setupMain(async ({ effects }) => {
 *   const container = await sdk.SubContainer.of(effects, { imageId: 'main' }, mounts, 'main')
 *
 *   return sdk.Daemons.of(effects)
 *     .addDaemon('primary', {
 *       subcontainer: container,
 *       exec: { command: ['my-server', '--config', '/data/config.json'] },
 *       ready: {
 *         display: 'Server',
 *         fn: () => sdk.healthCheck.checkPortListening(effects, 8080, {
 *           successMessage: 'Server is ready',
 *           errorMessage: 'Server is not responding',
 *         }),
 *         gracePeriod: 30000, // 30 second startup grace period
 *       },
 *       requires: [],
 *     })
 * })
 * ```
 */

import { Signals } from "../../../base/lib/types"

import { HealthCheckResult } from "../health/checkFns"

import { Trigger } from "../trigger"
import * as T from "../../../base/lib/types"
import { SubContainer } from "../util/SubContainer"

import { promisify } from "node:util"
import * as CP from "node:child_process"

export { Daemon } from "./Daemon"
export { CommandController } from "./CommandController"
import { EXIT_SUCCESS, HealthDaemon } from "./HealthDaemon"
import { Daemon } from "./Daemon"
import { CommandController } from "./CommandController"
import { Oneshot } from "./Oneshot"

/** @internal Promisified child process exec */
export const cpExec = promisify(CP.exec)
/** @internal Promisified child process execFile */
export const cpExecFile = promisify(CP.execFile)

/**
 * Configuration for a daemon's readiness/health check.
 *
 * Health checks determine when a daemon is considered "ready" and report
 * status to the StartOS UI. They run periodically and can be customized
 * with grace periods and triggers.
 */
export type Ready = {
  /**
   * Human-readable display name for the health check shown in the UI.
   * If null, the health check will not be visible in the UI.
   *
   * @example "Web Interface"
   * @example "Database Connection"
   */
  display: string | null

  /**
   * Function that determines the health status of the daemon.
   *
   * The SDK provides built-in health checks:
   * - `sdk.healthCheck.checkPortListening()` - Check if a port is listening
   * - `sdk.healthCheck.checkWebUrl()` - Check if an HTTP endpoint responds
   * - `sdk.healthCheck.runHealthScript()` - Run a custom health check script
   *
   * @returns HealthCheckResult with status ("success", "failure", or "starting") and message
   *
   * @example
   * ```typescript
   * fn: () => sdk.healthCheck.checkPortListening(effects, 8080, {
   *   successMessage: 'Server is ready',
   *   errorMessage: 'Server is not responding',
   * })
   * ```
   *
   * @example
   * ```typescript
   * // Custom health check
   * fn: async () => {
   *   const result = await container.exec(['my-health-check'])
   *   return result.exitCode === 0
   *     ? { result: 'success', message: 'Healthy' }
   *     : { result: 'failure', message: 'Unhealthy' }
   * }
   * ```
   */
  fn: () => Promise<HealthCheckResult> | HealthCheckResult

  /**
   * Duration in milliseconds to treat a failing health check as "starting" instead of "failure".
   *
   * This gives the daemon time to initialize before health check failures are reported.
   * After the grace period expires, failures will be reported normally.
   *
   * @default 5000 (5 seconds)
   *
   * @example 30000 // 30 second startup time
   * @example 120000 // 2 minutes for slow-starting services
   */
  gracePeriod?: number

  /**
   * Optional trigger configuration for when to run the health check.
   * If not specified, uses the default trigger (periodic checks).
   *
   * @see defaultTrigger, cooldownTrigger, changeOnFirstSuccess, successFailure
   */
  trigger?: Trigger
}

/**
 * Options for executing a command as a daemon.
 */
export type ExecCommandOptions = {
  /** The command to execute (string, array, or UseEntrypoint) */
  command: T.CommandType

  /**
   * Timeout in milliseconds to wait for graceful shutdown after sending SIGTERM.
   * After this timeout, SIGKILL will be sent.
   * @default 30000 (30 seconds)
   */
  sigtermTimeout?: number

  /**
   * If true, run the command as PID 1 (init process).
   * This affects signal handling and zombie process reaping.
   */
  runAsInit?: boolean

  /** Environment variables to set for the process */
  env?:
    | {
        [variable in string]?: string
      }
    | undefined

  /** Working directory for the process */
  cwd?: string | undefined

  /** User to run the process as (e.g., "root", "nobody") */
  user?: string | undefined

  /**
   * Callback invoked for each chunk of stdout output.
   * Useful for logging or monitoring process output.
   */
  onStdout?: (chunk: Buffer | string | any) => void

  /**
   * Callback invoked for each chunk of stderr output.
   * Useful for logging or monitoring process errors.
   */
  onStderr?: (chunk: Buffer | string | any) => void
}

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

/** @internal Helper to create a CommandController */
export const runCommand = <Manifest extends T.SDKManifest>() =>
  CommandController.of<Manifest, SubContainer<Manifest>>()

/**
 * Manager class for defining and controlling service daemons.
 *
 * Exposed via `sdk.Daemons`. Daemons are long-running processes that make up
 * your service. The Daemons class provides a fluent API for:
 * - Defining multiple daemons with dependencies between them
 * - Configuring health checks for each daemon
 * - Managing startup order based on dependency requirements
 * - Handling graceful shutdown in reverse dependency order
 *
 * @typeParam Manifest - The service manifest type
 * @typeParam Ids - Union type of all daemon IDs (accumulates as daemons are added)
 *
 * @example
 * ```typescript
 * // Single daemon service
 * return sdk.Daemons.of(effects)
 *   .addDaemon('primary', {
 *     subcontainer,
 *     exec: { command: sdk.useEntrypoint() },
 *     ready: {
 *       display: 'Server',
 *       fn: () => sdk.healthCheck.checkPortListening(effects, 8080, { ... }),
 *     },
 *     requires: [],
 *   })
 * ```
 *
 * @example
 * ```typescript
 * // Multi-daemon service with dependencies
 * return sdk.Daemons.of(effects)
 *   .addDaemon('database', {
 *     subcontainer: dbContainer,
 *     exec: { command: ['postgres', '-D', '/data'] },
 *     ready: { display: 'Database', fn: checkDbReady },
 *     requires: [],  // No dependencies
 *   })
 *   .addDaemon('api', {
 *     subcontainer: apiContainer,
 *     exec: { command: ['node', 'server.js'] },
 *     ready: { display: 'API Server', fn: checkApiReady },
 *     requires: ['database'],  // Waits for database to be ready
 *   })
 *   .addDaemon('worker', {
 *     subcontainer: workerContainer,
 *     exec: { command: ['node', 'worker.js'] },
 *     ready: { display: 'Background Worker', fn: checkWorkerReady },
 *     requires: ['database', 'api'],  // Waits for both
 *   })
 * ```
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
        "daemon" in options
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
      "__RUN_UNTIL_SUCCESS",
      "EXIT_SUCCESS",
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
          "Dependency cycle detected, shutting down remaining daemons",
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

  async build() {
    for (const daemon of this.healthDaemons) {
      await daemon.updateStatus()
    }
    return this
  }
}
