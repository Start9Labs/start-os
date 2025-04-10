import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGTERM } from "../../../base/lib/types"

import * as T from "../../../base/lib/types"
import {
  MountOptions,
  SubContainerHandle,
  SubContainer,
} from "../util/SubContainer"
import { Drop, splitCommand } from "../util"
import * as cp from "child_process"
import * as fs from "node:fs/promises"

export class CommandController extends Drop {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    private state: { exited: boolean },
    private readonly subcontainer: SubContainer,
    private process: cp.ChildProcess,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {
    super()
  }
  static of<Manifest extends T.SDKManifest>() {
    return async <A extends string>(
      effects: T.Effects,
      subcontainer:
        | {
            imageId: keyof Manifest["images"] & T.ImageId
            sharedRun?: boolean
          }
        | SubContainer,
      command: T.CommandType,
      options: {
        subcontainerName?: string
        // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
        sigtermTimeout?: number
        mounts?: { mountpoint: string; options: MountOptions }[]
        runAsInit?: boolean
        env?:
          | {
              [variable: string]: string
            }
          | undefined
        cwd?: string | undefined
        user?: string | undefined
        onStdout?: (chunk: Buffer | string | any) => void
        onStderr?: (chunk: Buffer | string | any) => void
      },
    ) => {
      let commands: string[]
      if (command instanceof T.UseEntrypoint) {
        const imageMeta: T.ImageMetadata = await fs
          .readFile(`/media/startos/images/${subcontainer.imageId}.json`, {
            encoding: "utf8",
          })
          .catch(() => "{}")
          .then(JSON.parse)
        commands = imageMeta.entrypoint ?? []
        commands.concat(...(command.overridCmd ?? imageMeta.cmd ?? []))
      } else commands = splitCommand(command)
      const subc =
        subcontainer instanceof SubContainer
          ? subcontainer
          : await (async () => {
              const subc = await SubContainer.of(
                effects,
                subcontainer,
                options?.subcontainerName || commands.join(" "),
              )
              try {
                for (let mount of options.mounts || []) {
                  await subc.mount(mount.options, mount.mountpoint)
                }
                return subc
              } catch (e) {
                await subc.destroy()
                throw e
              }
            })()

      try {
        let childProcess: cp.ChildProcess
        if (options.runAsInit) {
          childProcess = await subc.launch(commands, {
            env: options.env,
          })
        } else {
          childProcess = await subc.spawn(commands, {
            env: options.env,
            stdio: options.onStdout || options.onStderr ? "pipe" : "inherit",
          })
        }

        if (options.onStdout) childProcess.stdout?.on("data", options.onStdout)
        if (options.onStderr) childProcess.stderr?.on("data", options.onStderr)

        const state = { exited: false }
        const answer = new Promise<null>((resolve, reject) => {
          childProcess.on("exit", (code) => {
            state.exited = true
            if (
              code === 0 ||
              code === 143 ||
              (code === null && childProcess.signalCode == "SIGTERM")
            ) {
              return resolve(null)
            }
            if (code) {
              return reject(
                new Error(`${commands[0]} exited with code ${code}`),
              )
            } else {
              return reject(
                new Error(
                  `${commands[0]} exited with signal ${childProcess.signalCode}`,
                ),
              )
            }
          })
        })

        return new CommandController(
          answer,
          state,
          subc,
          childProcess,
          options.sigtermTimeout,
        )
      } catch (e) {
        await subc.destroy()
        throw e
      }
    }
  }
  get subContainerHandle() {
    return new SubContainerHandle(this.subcontainer)
  }
  async wait({ timeout = NO_TIMEOUT } = {}) {
    if (timeout > 0)
      setTimeout(() => {
        this.term()
      }, timeout)
    try {
      return await this.runningAnswer
    } finally {
      if (!this.state.exited) {
        this.process.kill("SIGKILL")
      }
      await this.subcontainer.destroy().catch((_) => {})
    }
  }
  async term({ signal = SIGTERM, timeout = this.sigtermTimeout } = {}) {
    try {
      if (!this.state.exited) {
        if (signal !== "SIGKILL") {
          setTimeout(() => {
            if (!this.state.exited) this.process.kill("SIGKILL")
          }, timeout)
        }
        if (!this.process.kill(signal)) {
          console.error(
            `failed to send signal ${signal} to pid ${this.process.pid}`,
          )
        }
      }

      await this.runningAnswer
    } finally {
      await this.subcontainer.destroy()
    }
  }
  onDrop(): void {
    this.term().catch(console.error)
  }
}
