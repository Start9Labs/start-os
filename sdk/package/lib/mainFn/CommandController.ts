import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGKILL, SIGTERM } from "../../../base/lib/types"

import * as T from "../../../base/lib/types"
import { asError } from "../../../base/lib/util/asError"
import {
  ExecSpawnable,
  MountOptions,
  SubContainerHandle,
  SubContainer,
} from "../util/SubContainer"
import { splitCommand } from "../util"
import * as cp from "child_process"

export class CommandController {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    private state: { exited: boolean },
    private readonly subcontainer: SubContainer,
    private process: cp.ChildProcessWithoutNullStreams,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {}
  static of<Manifest extends T.Manifest>() {
    return async <A extends string>(
      effects: T.Effects,
      subcontainer:
        | {
            id: keyof Manifest["images"] & T.ImageId
            sharedRun?: boolean
          }
        | SubContainer,
      command: T.CommandType,
      options: {
        subcontainerName?: string
        // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
        sigtermTimeout?: number
        mounts?: { path: string; options: MountOptions }[]
        runAsInit?: boolean
        env?:
          | {
              [variable: string]: string
            }
          | undefined
        cwd?: string | undefined
        user?: string | undefined
        onStdout?: (x: Buffer) => void
        onStderr?: (x: Buffer) => void
      },
    ) => {
      const commands = splitCommand(command)
      const subc =
        subcontainer instanceof SubContainer
          ? subcontainer
          : await (async () => {
              const subc = await SubContainer.of(
                effects,
                subcontainer,
                options?.subcontainerName || commands.join(" "),
              )
              for (let mount of options.mounts || []) {
                await subc.mount(mount.options, mount.path)
              }
              return subc
            })()
      let childProcess: cp.ChildProcessWithoutNullStreams
      if (options.runAsInit) {
        childProcess = await subc.launch(commands, {
          env: options.env,
        })
      } else {
        childProcess = await subc.spawn(commands, {
          env: options.env,
        })
      }
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
            return reject(new Error(`${commands[0]} exited with code ${code}`))
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
      await this.subcontainer.destroy?.().catch((_) => {})
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
      await this.subcontainer.destroy?.()
    }
  }
}
