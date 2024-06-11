import { NO_TIMEOUT, SIGKILL, SIGTERM, Signals } from "../StartSdk"
import { HealthReceipt } from "../health/HealthReceipt"
import { CheckResult } from "../health/checkFns"
import { SDKManifest } from "../manifest/ManifestTypes"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import {
  DaemonReturned,
  Effects,
  ImageId,
  ValidIfNoStupidEscape,
} from "../types"
import { Mounts } from "./Mounts"
import { CommandOptions, MountOptions, Overlay } from "../util/Overlay"
import { splitCommand } from "../util/splitCommand"

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
  fn: () => Promise<CheckResult> | CheckResult
  trigger?: Trigger
}

type DaemonsParams<
  Manifest extends SDKManifest,
  Ids extends string,
  Command extends string,
  Id extends string,
> = {
  command: ValidIfNoStupidEscape<Command> | [string, ...string[]]
  image: { id: keyof Manifest["images"] & ImageId; sharedRun?: boolean }
  mounts: Mounts<Manifest>
  env?: Record<string, string>
  ready: Ready
  requires: Exclude<Ids, Id>[]
}

type ErrorDuplicateId<Id extends string> = `The id '${Id}' is already used`

export const runCommand = <Manifest extends SDKManifest>() =>
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
export class Daemons<Manifest extends SDKManifest, Ids extends string> {
  private constructor(
    readonly effects: Effects,
    readonly started: (onTerm: () => PromiseLike<void>) => PromiseLike<void>,
    readonly daemons: Promise<Daemon>[],
    readonly ids: Ids[],
    readonly healthDaemons: HealthDaemon[],
  ) {}
  /**
   * Returns an empty new Daemons class with the provided config.
   *
   * Call .addDaemon() on the returned class to add a daemon.
   *
   * Daemons run in the order they are defined, with latter daemons being capable of
   * depending on prior daemons
   * @param config
   * @returns
   */
  static of<Manifest extends SDKManifest>(config: {
    effects: Effects
    started: (onTerm: () => PromiseLike<void>) => PromiseLike<void>
    healthReceipts: HealthReceipt[]
  }) {
    return new Daemons<Manifest, never>(
      config.effects,
      config.started,
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
    return {
      term: async (options?: { signal?: Signals; timeout?: number }) => {
        try {
          await Promise.all(this.healthDaemons.map((x) => x.term(options)))
        } finally {
          this.effects.setMainStatus({ status: "stopped" })
        }
      },
    }
  }

  private updateMainHealth() {
    if (this.healthDaemons.every((x) => x.health.status === "success")) {
      this.effects.setMainStatus({ status: "running" })
    } else {
      this.effects.setMainStatus({ status: "starting" })
    }
  }
}
