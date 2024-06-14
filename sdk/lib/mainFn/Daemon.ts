import { SDKManifest } from "../manifest/ManifestTypes"
import { Effects, ImageId, ValidIfNoStupidEscape } from "../types"
import { MountOptions, Overlay } from "../util/Overlay"
import { CommandController } from "./CommandController"

const TIMEOUT_INCREMENT_MS = 1000
const MAX_TIMEOUT_MS = 30000
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
        id: keyof Manifest["images"] & ImageId
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
      console.log("BLUJ Daemon.of", { imageId, command })
      const startCommand = () =>
        CommandController.of<Manifest>()(effects, imageId, command, options)
      return new Daemon(startCommand)
    }
  }

  async start() {
    console.log("BLUJ Daemon.start")
    if (this.commandController) {
      return
    }
    this.shouldBeRunning = true
    let timeoutCounter = 0
    new Promise(async () => {
      while (this.shouldBeRunning) {
        this.commandController = await this.startCommand()
        await this.commandController.wait().catch((err) => console.error(err))
        await new Promise((resolve) => setTimeout(resolve, timeoutCounter))
        timeoutCounter += TIMEOUT_INCREMENT_MS
        timeoutCounter = Math.max(MAX_TIMEOUT_MS, timeoutCounter)
      }
    }).catch((err) => {
      console.error(err)
    })
  }
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    console.log("BLUJ Daemon.term", termOptions)
    return this.stop(termOptions)
  }
  async stop(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    console.log("BLUJ Daemon.stop", termOptions)
    this.shouldBeRunning = false
    await this.commandController
      ?.term(termOptions)
      .catch((e) => console.error(e))
    this.commandController = null
  }
}
