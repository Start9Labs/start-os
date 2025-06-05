import { Signals } from "../../../base/lib/types"

import { HealthCheckResult } from "../health/checkFns"

import { Trigger } from "../trigger"
import * as T from "../../../base/lib/types"
import { Mounts } from "./Mounts"
import { MountOptions, SubContainer } from "../util/SubContainer"

import { promisify } from "node:util"
import * as CP from "node:child_process"

export { Daemon } from "./Daemon"
export { CommandController } from "./CommandController"
import { HealthDaemon } from "./HealthDaemon"
import { Daemon } from "./Daemon"
import { CommandController } from "./CommandController"
import { HealthCheck } from "../health/HealthCheck"
import { Oneshot } from "./Oneshot"
import { Manifest } from "../test/output.sdk"

export const cpExec = promisify(CP.exec)
export const cpExecFile = promisify(CP.execFile)
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
  fn: (
    subcontainer: SubContainer<Manifest>,
  ) => Promise<HealthCheckResult> | HealthCheckResult
  /**
   * A duration in milliseconds to treat a failing health check as "starting"
   *
   * defaults to 5000
   */
  gracePeriod?: number
  trigger?: Trigger
}

type NewDaemonParams<Manifest extends T.SDKManifest> = {
  /** The command line command to start the daemon */
  command:
    | T.CommandType
    | ((subcontainer: SubContainer<Manifest>) => Promise<T.CommandType>)
  /** Information about the subcontainer in which the daemon runs */
  subcontainer: SubContainer<Manifest>
  runAsInit?: boolean
  env?: Record<string, string>
  cwd?: string
  user?: string
  sigtermTimeout?: number
  onStdout?: (chunk: Buffer | string | any) => void
  onStderr?: (chunk: Buffer | string | any) => void
}

type AddDaemonParams<
  Manifest extends T.SDKManifest,
  Ids extends string,
  Id extends string,
> = (
  | NewDaemonParams<Manifest>
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
> = NewDaemonParams<Manifest> & {
  /** An array of IDs of prior daemons whose successful initializations are required before this daemon will initialize */
  requires: Exclude<Ids, Id>[]
}

type ErrorDuplicateId<Id extends string> = `The id '${Id}' is already used`

export const runCommand = <Manifest extends T.SDKManifest>() =>
  CommandController.of<Manifest>()

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
    readonly started: (onTerm: () => PromiseLike<void>) => PromiseLike<null>,
    readonly daemons: Promise<Daemon<Manifest>>[],
    readonly ids: Ids[],
    readonly healthDaemons: HealthDaemon<Manifest>[],
    readonly healthChecks: HealthCheck[],
  ) {}
  /**
   * Returns an empty new Daemons class with the provided inputSpec.
   *
   * Call .addDaemon() on the returned class to add a daemon.
   *
   * Daemons run in the order they are defined, with latter daemons being capable of
   * depending on prior daemons
   * @param options
   * @returns
   */
  static of<Manifest extends T.SDKManifest>(options: {
    effects: T.Effects
    started: (onTerm: () => PromiseLike<void>) => PromiseLike<null>
    healthChecks: HealthCheck[]
  }) {
    return new Daemons<Manifest, never>(
      options.effects,
      options.started,
      [],
      [],
      [],
      options.healthChecks,
    )
  }
  /**
   * Returns the complete list of daemons, including the one defined here
   * @param id
   * @param options
   * @returns a new Daemons object
   */
  addDaemon<Id extends string>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: AddDaemonParams<Manifest, Ids, Id>,
  ) {
    const daemon =
      "daemon" in options
        ? Promise.resolve(options.daemon)
        : Daemon.of<Manifest>()(
            this.effects,
            options.subcontainer,
            options.command,
            {
              ...options,
            },
          )
    const healthDaemon = new HealthDaemon(
      daemon,
      options.requires
        .map((x) => this.ids.indexOf(x))
        .filter((x) => x >= 0)
        .map((id) => this.healthDaemons[id]),
      id,
      this.ids,
      options.ready,
      this.effects,
    )
    const daemons = this.daemons.concat(daemon)
    const ids = [...this.ids, id] as (Ids | Id)[]
    const healthDaemons = [...this.healthDaemons, healthDaemon]
    return new Daemons<Manifest, Ids | Id>(
      this.effects,
      this.started,
      daemons,
      ids,
      healthDaemons,
      this.healthChecks,
    )
  }

  /**
   * Returns the complete list of daemons, including a "oneshot" daemon one defined here
   * a oneshot daemon is a command that executes once when started, and is considered "running" once it exits successfully
   * @param id
   * @param options
   * @returns a new Daemons object
   */
  addOneshot<Id extends string>(
    id: "" extends Id
      ? never
      : ErrorDuplicateId<Id> extends Id
        ? never
        : Id extends Ids
          ? ErrorDuplicateId<Id>
          : Id,
    options: AddOneshotParams<Manifest, Ids, Id>,
  ) {
    const daemon = Oneshot.of<Manifest>()(
      this.effects,
      options.subcontainer,
      options.command,
      {
        ...options,
      },
    )
    const healthDaemon = new HealthDaemon<Manifest>(
      daemon,
      options.requires
        .map((x) => this.ids.indexOf(x))
        .filter((x) => x >= 0)
        .map((id) => this.healthDaemons[id]),
      id,
      this.ids,
      "EXIT_SUCCESS",
      this.effects,
    )
    const daemons = this.daemons.concat(daemon)
    const ids = [...this.ids, id] as (Ids | Id)[]
    const healthDaemons = [...this.healthDaemons, healthDaemon]
    return new Daemons<Manifest, Ids | Id>(
      this.effects,
      this.started,
      daemons,
      ids,
      healthDaemons,
      this.healthChecks,
    )
  }

  async term() {
    try {
      this.healthChecks.forEach((health) => health.stop())
      for (let result of await Promise.allSettled(
        this.healthDaemons.map((x) => x.term()),
      )) {
        if (result.status === "rejected") {
          console.error(result.reason)
        }
      }
    } finally {
      this.effects.setMainStatus({ status: "stopped" })
    }
  }

  async build() {
    for (const daemon of this.healthDaemons) {
      await daemon.init()
    }
    for (const health of this.healthChecks) {
      health.start()
    }
    this.started(() => this.term())
    return this
  }
}
