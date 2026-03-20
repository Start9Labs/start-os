import * as T from '../../../base/lib/types'
import { asError } from '../../../base/lib/util/asError'
import { logErrorOnce } from '../../../base/lib/util/logErrorOnce'
import { Drop } from '../util'
import {
  SubContainer,
  SubContainerOwned,
  SubContainerRc,
} from '../util/SubContainer'
import { CommandController } from './CommandController'
import { DaemonCommandType } from './Daemons'
import { Oneshot } from './Oneshot'

const TIMEOUT_INCREMENT_MS = 1000
const MAX_TIMEOUT_MS = 30000
/**
 * A managed long-running process wrapper around {@link CommandController}.
 *
 * When started, the daemon automatically restarts its underlying command on failure
 * with exponential backoff (up to 30 seconds). When stopped, the command is terminated
 * gracefully. Implements {@link Drop} for automatic cleanup when the context is left.
 *
 * @typeParam Manifest - The service manifest type
 * @typeParam C - The subcontainer type, or `null` for JS-only daemons
 */
export class Daemon<
  Manifest extends T.SDKManifest,
  C extends SubContainer<Manifest> | null = SubContainer<Manifest> | null,
> extends Drop {
  private commandController: CommandController<Manifest, C> | null = null
  private shouldBeRunning = false
  protected exitedSuccess = false
  private exiting: Promise<void> | null = null
  private onExitFns: ((success: boolean) => void)[] = []
  protected constructor(
    private subcontainer: C,
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
   * The daemon auto-terminates when the effects context is left.
   */
  static of<Manifest extends T.SDKManifest>() {
    return <C extends SubContainer<Manifest> | null>(
      effects: T.Effects,
      subcontainer: C,
      exec: DaemonCommandType<Manifest, C>,
    ) => {
      let subc: SubContainer<Manifest> | null = subcontainer
      if (subcontainer && subcontainer.isOwned()) subc = subcontainer.rc()
      const startCommand = () =>
        CommandController.of<Manifest, C>()(
          effects,
          (subc?.rc() ?? null) as C,
          exec,
        )
      const res = new Daemon(subc, startCommand)
      effects.onLeaveContext(() => {
        res.term({ destroySubcontainer: true }).catch((e) => logErrorOnce(e))
      })
      return res
    }
  }
  /**
   * Start the daemon. If it is already running, this is a no-op.
   *
   * The daemon will automatically restart on failure with increasing backoff
   * until {@link term} is called.
   */
  async start() {
    if (this.commandController) {
      return
    }
    this.shouldBeRunning = true
    let timeoutCounter = 0
    ;(async () => {
      while (this.shouldBeRunning) {
        if (this.commandController)
          await this.commandController
            .term({})
            .catch((err) => logErrorOnce(err))
        try {
          this.commandController = await this.startCommand()
          if (!this.shouldBeRunning) {
            // handles race condition if stopped while starting
            await this.term()
            break
          }
          const success = await this.commandController.wait().then(
            (_) => true,
            (err) => {
              if (this.shouldBeRunning) logErrorOnce(err)
              return false
            },
          )
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
  /**
   * Terminate the daemon, stopping its underlying command.
   *
   * Sends the configured signal (default SIGTERM) and waits for the process to exit.
   * Optionally destroys the subcontainer after termination.
   *
   * @param termOptions - Optional termination settings
   * @param termOptions.signal - The signal to send (default: SIGTERM)
   * @param termOptions.timeout - Milliseconds to wait before SIGKILL
   * @param termOptions.destroySubcontainer - Whether to destroy the subcontainer after exit
   */
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
    destroySubcontainer?: boolean
  }) {
    this.shouldBeRunning = false
    this.exitedSuccess = false
    if (this.commandController) {
      this.exiting = this.commandController.term({ ...termOptions })
      this.commandController = null
      this.onExitFns = []
    }
    if (this.exiting) {
      await this.exiting.catch(logErrorOnce)
      if (termOptions?.destroySubcontainer) {
        await this.subcontainer?.destroy()
      }
      this.exiting = null
    }
  }
  /** Get a reference-counted handle to the daemon's subcontainer, or null if there is none */
  subcontainerRc(): SubContainerRc<Manifest> | null {
    return this.subcontainer?.rc() ?? null
  }
  /** Check whether this daemon shares the same subcontainer as another daemon */
  sharesSubcontainerWith(
    other: Daemon<Manifest, SubContainer<Manifest> | null>,
  ): boolean {
    return this.subcontainer?.guid === other.subcontainer?.guid
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
