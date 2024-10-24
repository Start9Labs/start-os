import { HealthReceipt, Signals } from "../../../base/lib/types"

import { HealthCheckResult } from "../health/checkFns"

import { Trigger } from "../trigger"
import * as T from "../../../base/lib/types"
import { Mounts } from "./Mounts"
import { ExecSpawnable, MountOptions } from "../util/SubContainer"

import { promisify } from "node:util"
import * as CP from "node:child_process"

export { Daemon } from "./Daemon"
export { CommandController } from "./CommandController"
import { HealthDaemon } from "./HealthDaemon"
import { Daemon } from "./Daemon"
import { CommandController } from "./CommandController"

export const cpExec = promisify(CP.exec)
export const cpExecFile = promisify(CP.execFile)
export type Ready = {
  display: string | null
  fn: (
    spawnable: ExecSpawnable,
  ) => Promise<HealthCheckResult> | HealthCheckResult
  trigger?: Trigger
}

type DaemonsParams<
  Manifest extends T.SDKManifest,
  Ids extends string,
  Command extends string,
  Id extends string,
> = {
  command: T.CommandType
  image: { id: keyof Manifest["images"] & T.ImageId; sharedRun?: boolean }
  mounts: Mounts<Manifest>
  env?: Record<string, string>
  ready: Ready
  requires: Exclude<Ids, Id>[]
  sigtermTimeout?: number
  onStdout?: (chunk: Buffer | string | any) => void
  onStderr?: (chunk: Buffer | string | any) => void
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
    readonly daemons: Promise<Daemon>[],
    readonly ids: Ids[],
    readonly healthDaemons: HealthDaemon[],
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
    healthReceipts: HealthReceipt[]
  }) {
    return new Daemons<Manifest, never>(
      options.effects,
      options.started,
      [],
      [],
      [],
    )
  }
  /**
   * Returns the complete list of daemons, including the one defined here
   * @param id
   * @param newDaemon
   * @returns
   */
  addDaemon<Id extends string, Command extends string>(
    // prettier-ignore
    id: 
      "" extends Id ? never :
      ErrorDuplicateId<Id> extends Id ? never :
      Id extends Ids ? ErrorDuplicateId<Id> :
      Id,
    options: DaemonsParams<Manifest, Ids, Command, Id>,
  ) {
    const daemonIndex = this.daemons.length
    const daemon = Daemon.of()(this.effects, options.image, options.command, {
      ...options,
      mounts: options.mounts.build(),
      subcontainerName: id,
    })
    const healthDaemon = new HealthDaemon(
      daemon,
      daemonIndex,
      options.requires
        .map((x) => this.ids.indexOf(id as any))
        .filter((x) => x >= 0)
        .map((id) => this.healthDaemons[id]),
      id,
      this.ids,
      options.ready,
      this.effects,
      options.sigtermTimeout,
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
    )
  }

  async build() {
    this.updateMainHealth()
    this.healthDaemons.forEach((x) =>
      x.addWatcher(() => this.updateMainHealth()),
    )
    const built = {
      term: async (options?: { signal?: Signals; timeout?: number }) => {
        try {
          await Promise.all(this.healthDaemons.map((x) => x.term(options)))
        } finally {
          this.effects.setMainStatus({ status: "stopped" })
        }
      },
    }
    this.started(() => built.term())
    return built
  }

  private updateMainHealth() {
    this.effects.setMainStatus({ status: "running" })
  }
}
