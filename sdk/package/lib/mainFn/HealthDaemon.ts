import { HealthCheckResult } from '../health/checkFns'
import { defaultTrigger } from '../trigger/defaultTrigger'
import { Ready } from './Daemons'
import { Daemon } from './Daemon'
import { SetHealth, Effects, SDKManifest } from '../../../base/lib/types'

export const EXIT_SUCCESS = 'EXIT_SUCCESS' as const

/**
 * Wanted a structure that deals with controlling daemons by their health status
 * States:
 * -- Waiting for dependencies to be success
 * -- Running: Daemon is running and the status is in the health
 *
 */
export class HealthDaemon<Manifest extends SDKManifest> {
  private _health: HealthCheckResult = { result: 'waiting', message: null }
  private healthWatchers: Array<() => unknown> = []
  private running = false
  private started?: number
  private resolveReady: (() => void) | undefined
  private resolvedReady: boolean = false
  private readyPromise: Promise<void>
  private session: { abort: AbortController; done: Promise<void> } | null = null
  constructor(
    readonly daemon: Daemon<Manifest> | null,
    readonly dependencies: HealthDaemon<Manifest>[],
    readonly id: string,
    readonly ready: Ready | typeof EXIT_SUCCESS,
    readonly effects: Effects,
  ) {
    this.readyPromise = new Promise(
      (resolve) =>
        (this.resolveReady = () => {
          resolve()
          this.resolvedReady = true
        }),
    )
    this.dependencies.forEach((d) => d.addWatcher(() => this.updateStatus()))
  }

  /** Run after we want to do cleanup */
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
    destroySubcontainer?: boolean
  }) {
    this.healthWatchers = []
    this.running = false
    await this.stopSession()

    await this.daemon?.term({
      ...termOptions,
    })
  }

  /** Want to add another notifier that the health might have changed */
  addWatcher(watcher: () => unknown) {
    this.healthWatchers.push(watcher)
  }

  get health() {
    return Object.freeze(this._health)
  }

  private async changeRunning(newStatus: boolean) {
    if (this.running === newStatus) return

    this.running = newStatus

    if (newStatus) {
      console.debug(`Launching ${this.id}...`)
      this.startSession()
      this.daemon?.start()
      this.started = performance.now()
    } else {
      console.debug(`Stopping ${this.id}...`)
      await this.stopSession()
      await this.daemon?.term()
    }
  }

  private async stopSession() {
    if (!this.session) return
    this.session.abort.abort()
    await this.session.done
    this.session = null
    this.resetReady()
  }

  private resetReady() {
    this.resolvedReady = false
    this.readyPromise = new Promise(
      (resolve) =>
        (this.resolveReady = () => {
          resolve()
          this.resolvedReady = true
        }),
    )
  }

  private startSession() {
    this.session?.abort.abort()

    const abort = new AbortController()

    this.daemon?.onExit((success) => {
      if (abort.signal.aborted) return
      if (success && this.ready === 'EXIT_SUCCESS') {
        this.setHealth({ result: 'success', message: null })
      } else if (!success) {
        this.setHealth({
          result: 'failure',
          message: `${this.id} daemon crashed`,
        })
      } else if (!this.daemon?.isOneshot()) {
        this.setHealth({
          result: 'failure',
          message: `${this.id} daemon exited`,
        })
      }
    })

    const done =
      this.ready === 'EXIT_SUCCESS'
        ? Promise.resolve()
        : this.runHealthCheckLoop(abort.signal)

    this.session = { abort, done }
  }

  private async runHealthCheckLoop(signal: AbortSignal): Promise<void> {
    if (this.ready === 'EXIT_SUCCESS') return
    const trigger = (this.ready.trigger ?? defaultTrigger)(() => ({
      lastResult: this._health.result,
    }))

    const aborted = new Promise<{ done: true }>((resolve) =>
      signal.addEventListener('abort', () => resolve({ done: true }), {
        once: true,
      }),
    )

    try {
      for (
        let res = await Promise.race([aborted, trigger.next()]);
        !res.done;
        res = await Promise.race([aborted, trigger.next()])
      ) {
        const response: HealthCheckResult = await Promise.resolve(
          this.ready.fn(),
        ).catch((err) => {
          return {
            result: 'failure' as const,
            message: 'message' in err ? err.message : String(err),
          }
        })

        if (signal.aborted) break
        await this.setHealth(response)
      }
    } catch (err) {
      if (!signal.aborted) {
        console.error(`Daemon ${this.id} health check failed: ${err}`)
      }
    }
  }

  onReady() {
    return this.readyPromise
  }

  get isReady() {
    return this.resolvedReady
  }

  private async setHealth(health: HealthCheckResult) {
    const changed = this._health.result !== health.result
    this._health = health
    if (this.resolveReady && health.result === 'success') {
      this.resolveReady()
    }
    if (changed) this.healthWatchers.forEach((watcher) => watcher())
    if (this.ready === 'EXIT_SUCCESS') return
    const display = this.ready.display
    if (!display) {
      return
    }
    let result = health.result
    if (
      result === 'failure' &&
      this.started &&
      performance.now() - this.started <= (this.ready.gracePeriod ?? 10_000)
    )
      result = 'starting'
    if (result === 'failure') {
      console.error(`Health Check ${this.id} failed:`, health.message)
    }
    await this.effects.setHealth({
      ...health,
      id: this.id,
      name: display,
      result,
    } as SetHealth)
  }

  async updateStatus() {
    const healths = this.dependencies.map((d) => ({
      health: d.running && d._health,
      id: d.id,
      display: typeof d.ready === 'object' ? d.ready.display : null,
    }))
    const waitingOn = healths.filter(
      (h) => !h.health || h.health.result !== 'success',
    )
    if (waitingOn.length)
      console.debug(
        `daemon ${this.id} waiting on ${waitingOn.map((w) => w.id)}`,
      )
    if (waitingOn.length) {
      const waitingOnNames = waitingOn.flatMap((w) =>
        w.display ? [w.display] : [],
      )
      const message = waitingOnNames.length ? waitingOnNames.join(', ') : null
      await this.setHealth({ result: 'waiting', message })
    } else {
      await this.setHealth({ result: 'starting', message: null })
    }
    await this.changeRunning(!waitingOn.length)
  }
}
