import * as T from '../../../base/lib/types'
import { asError } from '../../../base/lib/util/asError'
import { logErrorOnce } from '../../../base/lib/util/logErrorOnce'
import { Drop } from '../util'
import { SubContainer } from '../util/SubContainer'
import { CommandController } from './CommandController'
import { DaemonCommandType } from './Daemons'
import { Oneshot } from './Oneshot'

const TIMEOUT_INCREMENT_MS = 1000
const MAX_TIMEOUT_MS = 30000

/**
 * A managed long-running process wrapper around {@link CommandController}.
 *
 * When started, the daemon automatically restarts its underlying command on
 * failure with exponential backoff (up to 30 seconds). When stopped, the
 * command is terminated gracefully. While the daemon is running, it holds a
 * use-token (`subcontainer.hold()`) on its SubContainer so the container is
 * not destroyed out from under it. The hold is released on `term()`.
 *
 * @typeParam Manifest - The service manifest type
 * @typeParam C - The subcontainer type, or `null` for JS-only daemons
 */
export class Daemon<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null = SubContainer<Manifest> | null,
> extends Drop {
  private commandController: CommandController<Manifest, C> | null = null
  protected exitedSuccess = false
  private onExitFns: ((success: boolean) => void)[] = []
  private loop: { abort: AbortController; done: Promise<void> } | null = null
  private releaseSubcontainerHold: (() => Promise<void>) | null = null
  protected constructor(
    readonly subcontainer: C,
    private startCommand: () => Promise<CommandController<Manifest, C>>,
    readonly oneshot: boolean = false,
  ) {
    super()
  }
  /** Returns true if this daemon is a one-shot process (exits after success) */
  isOneshot(): this is Oneshot<Manifest> {
    return this.oneshot
  }
  /**
   * Factory method to create a new Daemon.
   *
   * Returns a curried function: `(effects, subcontainer, exec) => Daemon`.
   * Termination on context-leave is handled by the owning `Daemons` instance
   * (which calls `term()` in dependent-first order); standalone daemons
   * created outside `Daemons` are responsible for their own teardown.
   */
  static of<Manifest extends T.SDKManifest>() {
    return <C extends SubContainer<Manifest> | null>(
      effects: T.Effects,
      subcontainer: C,
      exec: DaemonCommandType<Manifest, C>,
    ) => {
      const startCommand = () =>
        CommandController.of<Manifest, C>()(effects, subcontainer, exec)
      return new Daemon<Manifest, C>(subcontainer, startCommand)
    }
  }
  /**
   * Start the daemon. If it is already running, this is a no-op.
   *
   * Takes a `hold()` on the daemon's SubContainer (if any) so the container
   * is kept alive while the daemon runs. The hold is released on
   * {@link term}. The daemon will automatically restart on failure with
   * increasing backoff until `term` is called.
   */
  async start() {
    if (this.loop) {
      return
    }
    if (this.subcontainer && !this.releaseSubcontainerHold) {
      this.releaseSubcontainerHold = this.subcontainer.hold()
    }
    const abort = new AbortController()
    const done = this.runLoop(abort.signal)
    this.loop = { abort, done }
  }

  private async runLoop(signal: AbortSignal) {
    let timeoutCounter = 0
    try {
      while (!signal.aborted) {
        if (this.commandController) {
          await this.commandController.term({}).catch(logErrorOnce)
          this.commandController = null
        }
        try {
          this.commandController = await this.startCommand()
          if (signal.aborted) {
            await this.commandController.term({}).catch(logErrorOnce)
            this.commandController = null
            break
          }
          const success = await this.commandController.wait().then(
            (_) => true,
            (err) => {
              if (!signal.aborted) logErrorOnce(err)
              return false
            },
          )
          this.commandController = null
          if (signal.aborted) break
          for (const fn of this.onExitFns) {
            try {
              fn(success)
            } catch (e) {
              console.error('EXIT handler', e)
            }
          }
          if (success && this.oneshot) {
            this.exitedSuccess = true
            break
          }
        } catch (e) {
          if (!signal.aborted) console.error(e)
        }
        if (signal.aborted) break
        await new Promise<void>((resolve) => {
          const timer = setTimeout(resolve, timeoutCounter)
          signal.addEventListener(
            'abort',
            () => {
              clearTimeout(timer)
              resolve()
            },
            { once: true },
          )
        })
        timeoutCounter += TIMEOUT_INCREMENT_MS
        timeoutCounter = Math.min(MAX_TIMEOUT_MS, timeoutCounter)
      }
    } finally {
      this.loop = null
    }
  }

  /**
   * Terminate the daemon, stopping its underlying command and releasing
   * the SubContainer hold taken at {@link start}.
   *
   * Sends the configured signal (default SIGTERM) and waits for the
   * process to exit. The SubContainer itself is not destroyed by this call
   * — call `subcontainer.destroy()` separately (typically the owning
   * `Daemons` instance does that). The container is torn down by the
   * SubContainer's own machinery when destroy has been requested and all
   * outstanding holds have been released.
   *
   * @param termOptions - Optional termination settings
   * @param termOptions.signal - The signal to send (default: SIGTERM)
   * @param termOptions.timeout - Milliseconds to wait before SIGKILL
   */
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    this.exitedSuccess = false
    this.onExitFns = []

    if (this.loop) {
      this.loop.abort.abort()
    }

    const exiting = this.commandController?.term({ ...termOptions })
    this.commandController = null
    if (exiting) await exiting.catch(logErrorOnce)

    if (this.loop) {
      await this.loop.done
    }

    if (this.releaseSubcontainerHold) {
      const release = this.releaseSubcontainerHold
      this.releaseSubcontainerHold = null
      await release().catch(logErrorOnce)
    }
  }

  /**
   * Register a callback to be invoked each time the daemon's process exits.
   * @param fn - Callback receiving `true` on clean exit, `false` on error
   */
  onExit(fn: (success: boolean) => void) {
    this.onExitFns.push(fn)
  }
  onDrop(): void {
    this.term().catch((e) => logErrorOnce(asError(e)))
  }
}
