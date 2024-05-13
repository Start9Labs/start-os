import { NO_TIMEOUT, SIGKILL, SIGTERM } from "../StartSdk"
import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ValidIfNoStupidEscape } from "../types"
import { MountOptions, Overlay } from "../util/Overlay"
import { splitCommand } from "../util/splitCommand"
import { cpExecFile, cpExec } from "./Daemons"

export class CommandController {
  private constructor(
    readonly runningAnswer: Promise<unknown>,
    readonly overlay: Overlay,
    readonly pid: number | undefined,
  ) {}
  static of<Manifest extends SDKManifest>() {
    return async <A extends string>(
      effects: Effects,
      imageId: {
        id: Manifest["images"][number]
        sharedRun?: boolean
      },
      command: ValidIfNoStupidEscape<A> | [string, ...string[]],
      options: {
        mounts?: { path: string; options: MountOptions }[]
        overlay?: Overlay
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
      const overlay = options.overlay || (await Overlay.of(effects, imageId))
      for (let mount of options.mounts || []) {
        await overlay.mount(mount.options, mount.path)
      }
      const childProcess = await overlay.spawn(commands, {
        env: options.env,
      })
      const answer = new Promise<null>((resolve, reject) => {
        childProcess.stdout.on(
          "data",
          options.onStdout ??
            ((data: any) => {
              console.log(data.toString())
            }),
        )
        childProcess.stderr.on(
          "data",
          options.onStderr ??
            ((data: any) => {
              console.error(data.toString())
            }),
        )

        childProcess.on("exit", (code: any) => {
          if (code === 0) {
            return resolve(null)
          }
          return reject(new Error(`${commands[0]} exited with code ${code}`))
        })
      })

      const pid = childProcess.pid

      return new CommandController(answer, overlay, pid)
    }
  }
  async wait() {
    try {
      return await this.runningAnswer
    } finally {
      if (this.pid !== undefined) {
        await cpExecFile("pkill", ["-9", "-s", String(this.pid)]).catch(
          (_) => {},
        )
      }
      await this.overlay.destroy().catch((_) => {})
    }
  }
  async term({ signal = SIGTERM, timeout = NO_TIMEOUT } = {}) {
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
      await this.overlay.destroy()
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
    let to = null
    if (timeout !== 0) {
      to = setTimeout(() => {
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
        if (to) {
          clearTimeout(to)
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
