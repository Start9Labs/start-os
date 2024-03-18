import { NO_TIMEOUT, SIGKILL, SIGTERM, Signals } from "../StartSdk"
import { HealthReceipt } from "../health/HealthReceipt"
import { CheckResult } from "../health/checkFns"
import { SDKManifest } from "../manifest/ManifestTypes"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { DaemonReturned, Effects, ValidIfNoStupidEscape } from "../types"
import { CommandOptions, MountOptions, Overlay } from "../util/Overlay"
import { splitCommand } from "../util/splitCommand"

import { promisify } from "node:util"
import * as CP from "node:child_process"

const cpExec = promisify(CP.exec)
const cpExecFile = promisify(CP.execFile)
async function psTree(pid: number, overlay: Overlay): Promise<number[]> {
  const { stdout } = await cpExec(`pstree -p ${pid}`)
  const regex: RegExp = /\((\d+)\)/g
  return [...stdout.toString().matchAll(regex)].map(([_all, pid]) =>
    parseInt(pid),
  )
}
type Daemon<
  Manifest extends SDKManifest,
  Ids extends string,
  Command extends string,
  Id extends string,
> = {
  id: "" extends Id ? never : Id
  command: ValidIfNoStupidEscape<Command> | [string, ...string[]]
  imageId: Manifest["images"][number]
  env?: Record<string, string>
  ready: {
    display: string | null
    fn: () => Promise<CheckResult> | CheckResult
    trigger?: Trigger
  }
  requires: Exclude<Ids, Id>[]
}

type ErrorDuplicateId<Id extends string> = `The id '${Id}' is already used`

const runDaemon =
  <Manifest extends SDKManifest>() =>
  async <A extends string>(
    effects: Effects,
    imageId: Manifest["images"][number],
    command: ValidIfNoStupidEscape<A> | [string, ...string[]],
    options: CommandOptions & {
      mounts?: { path: string; options: MountOptions }[]
      overlay?: Overlay
    },
  ): Promise<DaemonReturned> => {
    const commands = splitCommand(command)
    const overlay = options.overlay || (await Overlay.of(effects, imageId))
    for (let mount of options.mounts || []) {
      await overlay.mount(mount.options, mount.path)
    }
    const childProcess = await overlay.spawn(commands, {
      env: options.env,
    })
    const answer = new Promise<null>((resolve, reject) => {
      childProcess.stdout.on("data", (data: any) => {
        console.log(data.toString())
      })
      childProcess.stderr.on("data", (data: any) => {
        console.error(data.toString())
      })

      childProcess.on("exit", (code: any) => {
        if (code === 0) {
          return resolve(null)
        }
        return reject(new Error(`${commands[0]} exited with code ${code}`))
      })
    })

    const pid = childProcess.pid
    return {
      async wait() {
        const pids = pid ? await psTree(pid, overlay) : []
        try {
          return await answer
        } finally {
          for (const process of pids) {
            cpExecFile("kill", [`-9`, String(process)]).catch((_) => {})
          }
        }
      },
      async term({ signal = SIGTERM, timeout = NO_TIMEOUT } = {}) {
        const pids = pid ? await psTree(pid, overlay) : []
        try {
          childProcess.kill(signal)

          if (timeout > NO_TIMEOUT) {
            const didTimeout = await Promise.race([
              new Promise((resolve) => setTimeout(resolve, timeout)).then(
                () => true,
              ),
              answer.then(() => false),
            ])
            if (didTimeout) {
              childProcess.kill(SIGKILL)
            }
          } else {
            await answer
          }
        } finally {
          await overlay.destroy()
        }

        try {
          for (const process of pids) {
            await cpExecFile("kill", [`-${signal}`, String(process)])
          }
        } finally {
          for (const process of pids) {
            cpExecFile("kill", [`-9`, String(process)]).catch((_) => {})
          }
        }
      },
    }
  }

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
    readonly daemons?: Daemon<Manifest, Ids, "command", Ids>[],
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
    return new Daemons<Manifest, never>(config.effects, config.started)
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
    newDaemon: Omit<Daemon<Manifest, Ids, Command, Id>, "id">,
  ) {
    const daemons = ((this?.daemons ?? []) as any[]).concat({
      ...newDaemon,
      id,
    })
    return new Daemons<Manifest, Ids | Id>(this.effects, this.started, daemons)
  }

  async build() {
    const daemonsStarted = {} as Record<Ids, Promise<DaemonReturned>>
    const { effects } = this
    const daemons = this.daemons ?? []
    for (const daemon of daemons) {
      const requiredPromise = Promise.all(
        daemon.requires?.map((id) => daemonsStarted[id]) ?? [],
      )
      daemonsStarted[daemon.id] = requiredPromise.then(async () => {
        const { command, imageId } = daemon

        const child = runDaemon<Manifest>()(effects, imageId, command, {
          env: daemon.env,
        })
        let currentInput: TriggerInput = {}
        const getCurrentInput = () => currentInput
        const trigger = (daemon.ready.trigger ?? defaultTrigger)(
          getCurrentInput,
        )
        return new Promise(async (resolve) => {
          for (
            let res = await trigger.next();
            !res.done;
            res = await trigger.next()
          ) {
            const response = await Promise.resolve(daemon.ready.fn()).catch(
              (err) =>
                ({
                  status: "failure",
                  message: "message" in err ? err.message : String(err),
                }) as CheckResult,
            )
            currentInput.lastResult = response.status || null
            if (!currentInput.hadSuccess && response.status === "passing") {
              currentInput.hadSuccess = true
              resolve(child)
            }
          }
          resolve(child)
        })
      })
    }
    return {
      async term(options?: { signal?: Signals; timeout?: number }) {
        await Promise.all(
          Object.values<Promise<DaemonReturned>>(daemonsStarted).map((x) =>
            x.then((x) => x.term(options)),
          ),
        )
      },
      async wait() {
        await Promise.all(
          Object.values<Promise<DaemonReturned>>(daemonsStarted).map((x) =>
            x.then((x) => x.wait()),
          ),
        )
      },
    }
  }
}
