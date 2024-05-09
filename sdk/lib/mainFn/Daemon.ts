import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ValidIfNoStupidEscape } from "../types"
import { MountOptions, Overlay } from "../util/Overlay"
import { CommandController } from "./CommandController"

/**
 * This is a wrapper around CommandController that has a state of off, where the command shouldn't be running
 * and the others state of running, where it will keep a living running command
 */

export class Daemon {
  private commandController: CommandController | null = null
  private shouldBeRunning = false
  private constructor(private startCommand: () => Promise<CommandController>) {}
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
      const startCommand = () =>
        CommandController.of<Manifest>()(effects, imageId, command, options)
      return new Daemon(startCommand)
    }
  }

  async start() {
    if (this.commandController) {
      return
    }
    this.shouldBeRunning = true
    new Promise(async () => {
      while (this.shouldBeRunning) {
        this.commandController = await this.startCommand()
        await this.commandController.wait().catch((err) => console.error(err))
      }
    }).catch((err) => {
      console.error(err)
    })
  }
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    return this.stop(termOptions)
  }
  async stop(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    this.shouldBeRunning = false
    await this.commandController?.term(termOptions)
  }
}
