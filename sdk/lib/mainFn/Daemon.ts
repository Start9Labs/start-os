import * as T from "../types"
import { asError } from "../util/asError"
import { ExecSpawnable, MountOptions, SubContainer } from "../util/SubContainer"
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
  constructor(private startCommand: () => Promise<CommandController>) {}
  get subContainerHandle(): undefined | ExecSpawnable {
    return this.commandController?.subContainerHandle
  }
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
        mounts?: { path: string; options: MountOptions }[]
        env?:
          | {
              [variable: string]: string
            }
          | undefined
        cwd?: string | undefined
        user?: string | undefined
        onStdout?: (x: Buffer) => void
        onStderr?: (x: Buffer) => void
        sigtermTimeout?: number
      },
    ) => {
      const startCommand = () =>
        CommandController.of<Manifest>()(
          effects,
          subcontainer,
          command,
          options,
        )
      return new Daemon(startCommand)
    }
  }
  async start() {
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
      console.error(asError(err))
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
    await this.commandController
      ?.term({ ...termOptions })
      .catch((e) => console.error(asError(e)))
    this.commandController = null
  }
}