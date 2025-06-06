import * as T from "../../../base/lib/types"
import { asError } from "../../../base/lib/util/asError"
import { Drop } from "../util"
import {
  SubContainer,
  SubContainerOwned,
  SubContainerRc,
} from "../util/SubContainer"
import { CommandController } from "./CommandController"
import { DaemonCommandType } from "./Daemons"
import { Oneshot } from "./Oneshot"

const TIMEOUT_INCREMENT_MS = 1000
const MAX_TIMEOUT_MS = 30000
/**
 * This is a wrapper around CommandController that has a state of off, where the command shouldn't be running
 * and the others state of running, where it will keep a living running command
 */

export class Daemon<Manifest extends T.SDKManifest> extends Drop {
  private commandController: CommandController<Manifest> | null = null
  private shouldBeRunning = false
  protected exitedSuccess = false
  private onExitFns: ((success: boolean) => void)[] = []
  protected constructor(
    private subcontainer: SubContainer<Manifest>,
    private startCommand: (() => Promise<CommandController<Manifest>>) | null,
    readonly oneshot: boolean = false,
  ) {
    super()
  }
  isOneshot(): this is Oneshot<Manifest> {
    return this.oneshot
  }
  static of<Manifest extends T.SDKManifest>() {
    return async (
      effects: T.Effects,
      subcontainer: SubContainer<Manifest>,
      exec: DaemonCommandType | null,
    ) => {
      if (subcontainer.isOwned()) subcontainer = subcontainer.rc()
      const startCommand = exec
        ? () =>
            CommandController.of<Manifest>()(effects, subcontainer.rc(), exec)
        : null
      return new Daemon(subcontainer, startCommand)
    }
  }
  async start() {
    if (this.commandController) {
      return
    }
    this.shouldBeRunning = true
    let timeoutCounter = 0
    ;(async () => {
      while (this.startCommand && this.shouldBeRunning) {
        if (this.commandController)
          await this.commandController
            .term({})
            .catch((err) => console.error(err))
        try {
          this.commandController = await this.startCommand()
          const success = await this.commandController.wait().then(
            (_) => true,
            (err) => {
              console.error(err)
              return false
            },
          )
          for (const fn of this.onExitFns) {
            try {
              fn(success)
            } catch (e) {
              console.error("EXIT handler", e)
            }
          }
          if (success && this.oneshot) {
            this.exitedSuccess = true
            break
          }
        } catch (e) {
          console.error(e)
        }
        await new Promise((resolve) => setTimeout(resolve, timeoutCounter))
        timeoutCounter += TIMEOUT_INCREMENT_MS
        timeoutCounter = Math.min(MAX_TIMEOUT_MS, timeoutCounter)
      }
    })().catch((err) => {
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
    this.exitedSuccess = false
    await this.commandController
      ?.term({ ...termOptions })
      .catch((e) => console.error(asError(e)))
    this.commandController = null
    this.onExitFns = []
    await this.subcontainer.destroy()
  }
  subcontainerRc(): SubContainerRc<Manifest> {
    return this.subcontainer.rc()
  }
  onExit(fn: (success: boolean) => void) {
    this.onExitFns.push(fn)
  }
  onDrop(): void {
    this.stop().catch((e) => console.error(asError(e)))
  }
}
