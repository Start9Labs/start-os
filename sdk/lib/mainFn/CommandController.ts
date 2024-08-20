import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { NO_TIMEOUT, SIGKILL, SIGTERM } from "../StartSdk"

import * as T from "../types"
import { asError } from "../util/asError"
import {
  ExecSpawnable,
  MountOptions,
  NonDestroyableOverlay,
  SubContainer,
} from "../util/SubContainer"
import { splitCommand } from "../util/splitCommand"
import { cpExecFile, cpExec } from "./Daemons"

export class CommandController {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    private readonly overlay: ExecSpawnable,
    readonly pid: number | undefined,
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
        overlay?: ExecSpawnable
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
      const overlay =
        options.overlay ||
        (await (async () => {
          const overlay = await SubContainer.of(effects, imageId)
          for (let mount of options.mounts || []) {
            await overlay.mount(mount.options, mount.path)
          }
          return overlay
        })())
      const childProcess = await overlay.spawn(commands, {
        env: options.env,
      })
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

      const pid = childProcess.pid

      return new CommandController(answer, overlay, pid, options.sigtermTimeout)
    }
  }
  get nonDestroyableOverlay() {
    return new NonDestroyableOverlay(this.overlay)
  }
  async wait({ timeout = NO_TIMEOUT } = {}) {
    if (timeout > 0)
      setTimeout(() => {
        this.term()
      }, timeout)
    try {
      return await this.runningAnswer
    } finally {
      if (this.pid !== undefined) {
        await cpExecFile("pkill", ["-9", "-s", String(this.pid)]).catch(
          (_) => {},
        )
      }
      await this.overlay.destroy?.().catch((_) => {})
    }
  }
  async term({ signal = SIGTERM, timeout = this.sigtermTimeout } = {}) {
    if (this.pid === undefined) return
    try {
      await cpExecFile("pkill", [
        `-${signal.replace("SIG", "")}`,
        "-s",
        String(this.pid),
      ])

      const didTimeout = await waitSession(this.pid, timeout)
      if (didTimeout) {
        await cpExecFile("pkill", [`-9`, "-s", String(this.pid)]).catch(
          (_) => {},
        )
      }
    } finally {
      await this.overlay.destroy?.()
    }
  }
}

function waitSession(
  sid: number,
  timeout = NO_TIMEOUT,
  interval = 100,
): Promise<boolean> {
  let nextInterval = interval * 2
  if (timeout >= 0 && timeout < nextInterval) {
    nextInterval = timeout
  }
  let nextTimeout = timeout
  if (timeout > 0) {
    if (timeout >= interval) {
      nextTimeout -= interval
    } else {
      nextTimeout = 0
    }
  }
  return new Promise((resolve, reject) => {
    let next: NodeJS.Timeout | null = null
    if (timeout !== 0) {
      next = setTimeout(() => {
        waitSession(sid, nextTimeout, nextInterval).then(resolve, reject)
      }, interval)
    }
    cpExecFile("ps", [`--sid=${sid}`, "-o", "--pid="]).then(
      (_) => {
        if (timeout === 0) {
          resolve(true)
        }
      },
      (e) => {
        if (next) {
          clearTimeout(next)
        }
        if (typeof e === "object" && e && "code" in e && e.code) {
          resolve(false)
        } else {
          reject(e)
        }
      },
    )
  })
}
