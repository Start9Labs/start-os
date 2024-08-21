import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGKILL, SIGTERM } from "../StartSdk"

import * as T from "../types"
import { asError } from "../util/asError"
import {
  ExecSpawnable,
  MountOptions,
  SubContainerHandle,
  SubContainer,
} from "../util/SubContainer"
import { splitCommand } from "../util/splitCommand"
import { cpExecFile, cpExec } from "./Daemons"
import * as cp from "child_process"

export class CommandController {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    private readonly subcontainer: SubContainer,
    private process: cp.ChildProcessWithoutNullStreams | undefined,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {}
  static of<Manifest extends T.Manifest>() {
    return async <A extends string>(
      effects: T.Effects,
      imageId: {
        id: keyof Manifest["images"] & T.ImageId
        sharedRun?: boolean
      },
      command: T.CommandType,
      options: {
        // Defaults to the DEFAULT_SIGTERM_TIMEOUT = 30_000ms
        sigtermTimeout?: number
        mounts?: { path: string; options: MountOptions }[]
        subcontainer?: SubContainer
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
      const subcontainer =
        options.subcontainer ||
        (await (async () => {
          const subcontainer = await SubContainer.of(effects, imageId)
          for (let mount of options.mounts || []) {
            await subcontainer.mount(mount.options, mount.path)
          }
          return subcontainer
        })())
      let childProcess: cp.ChildProcessWithoutNullStreams
      if (options.runAsInit) {
        childProcess = await subcontainer.launch(commands, {
          env: options.env,
        })
      } else {
        childProcess = await subcontainer.spawn(commands, {
          env: options.env,
        })
      }
      const answer = new Promise<null>((resolve, reject) => {
        childProcess.on("exit", (code: any) => {
          if (code === 0) {
            return resolve(null)
          }
          return reject(
            asError(new Error(`${commands[0]} exited with code ${code}`)),
          )
        })
      })

      return new CommandController(
        answer,
        subcontainer,
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
      if (this.process !== undefined) {
        if (this.process.kill("SIGKILL")) {
          this.process = undefined
        }
      }
      await this.subcontainer.destroy?.().catch((_) => {})
    }
  }
  async term({ signal = SIGTERM, timeout = this.sigtermTimeout } = {}) {
    if (this.process === undefined) return
    try {
      if (!this.process.kill(signal)) {
        console.error(
          `failed to send signal ${signal} to pid ${this.process.pid}`,
        )
      }

      await this.wait({ timeout })
    } finally {
      await this.subcontainer.destroy?.()
    }
  }
}
