import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGTERM } from "../../../base/lib/types"

import * as T from "../../../base/lib/types"
import { MountOptions, SubContainer } from "../util/SubContainer"
import { Drop, splitCommand } from "../util"
import * as cp from "child_process"
import * as fs from "node:fs/promises"
import { Mounts } from "./Mounts"

export class CommandController<Manifest extends T.SDKManifest> extends Drop {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    private state: { exited: boolean },
    private readonly subcontainer: SubContainer<Manifest>,
    private process: cp.ChildProcess,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {
    super()
  }
  static of<Manifest extends T.SDKManifest>() {
    return async (
      effects: T.Effects,
      subcontainer: SubContainer<Manifest>,
      command: T.CommandType,
      options: {
        // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
        sigtermTimeout?: number
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
      try {
        let commands: string[]
        if (command instanceof T.UseEntrypoint) {
          const imageMeta: T.ImageMetadata = await fs
            .readFile(`/media/startos/images/${subcontainer.imageId}.json`, {
              encoding: "utf8",
            })
            .catch(() => "{}")
            .then(JSON.parse)
          commands = imageMeta.entrypoint ?? []
          commands = commands.concat(
            ...(command.overridCmd ?? imageMeta.cmd ?? []),
          )
        } else commands = splitCommand(command)

        let childProcess: cp.ChildProcess
        if (options.runAsInit) {
          childProcess = await subcontainer.launch(commands, {
            env: options.env,
          })
        } else {
          childProcess = await subcontainer.spawn(commands, {
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
          subcontainer,
          childProcess,
          options.sigtermTimeout,
        )
      } catch (e) {
        await subcontainer.destroy()
        throw e
      }
    }
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
      await this.subcontainer.destroy()
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
