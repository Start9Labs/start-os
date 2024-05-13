import { NO_TIMEOUT, SIGTERM } from "../StartSdk"
import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ValidIfNoStupidEscape } from "../types"
import { MountOptions, Overlay } from "../util/Overlay"
import { splitCommand } from "../util/splitCommand"
import { cpExecFile } from "./Daemons"

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
      await cpExecFile("pkill", ["-9", "-s", String(this.pid)]).catch((_) => {})
      await this.overlay.destroy().catch((_) => {})
    }
  }
  async term({ signal = SIGTERM, timeout = NO_TIMEOUT } = {}) {
    try {
      await cpExecFile("pkill", [
        `-${signal.replace("SIG", "")}`,
        "-s",
        String(this.pid),
      ])

      if (timeout > NO_TIMEOUT) {
        const didTimeout = await Promise.race([
          new Promise((resolve) => setTimeout(resolve, timeout)).then(
            () => true,
          ),
          this.runningAnswer.then(() => false),
        ])
        if (didTimeout) {
          await cpExecFile("pkill", [`-9`, "-s", String(this.pid)]).catch(
            (_: any) => {},
          )
        }
      } else {
        await this.runningAnswer
      }
    } finally {
      await this.overlay.destroy()
    }
  }
}
