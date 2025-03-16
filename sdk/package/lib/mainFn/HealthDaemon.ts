import { HealthCheckResult } from "../health/checkFns"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { Ready } from "./Daemons"
import { Daemon } from "./Daemon"
import { SetHealth, Effects } from "../../../base/lib/types"
import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { asError } from "../../../base/lib/util/asError"

const oncePromise = <T>() => {
  let resolve: (value: T) => void
  const promise = new Promise<T>((res) => {
    resolve = res
  })
  return { resolve: resolve!, promise }
}

/**
 * Wanted a structure that deals with controlling daemons by their health status
 * States:
 * -- Waiting for dependencies to be success
 * -- Running: Daemon is running and the status is in the health
 *
 */
export class HealthDaemon {
  private _health: HealthCheckResult = { result: "starting", message: null }
  private healthWatchers: Array<() => unknown> = []
  private running = false
  private started?: number
  private resolveReady: (() => void) | undefined
  private readyPromise: Promise<void>
  constructor(
    private readonly daemon: Promise<Daemon>,
    readonly daemonIndex: number,
    private readonly dependencies: HealthDaemon[],
    readonly id: string,
    readonly ids: string[],
    readonly ready: Ready,
    readonly effects: Effects,
    readonly sigtermTimeout: number = DEFAULT_SIGTERM_TIMEOUT,
  ) {
    this.readyPromise = new Promise((resolve) => (this.resolveReady = resolve))
    this.dependencies.forEach((d) => d.addWatcher(() => this.updateStatus()))
  }

  /** Run after we want to do cleanup */
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    this.healthWatchers = []
    this.running = false
    this.healthCheckCleanup?.()

    await this.daemon.then((d) =>
      d.term({
        timeout: this.sigtermTimeout,
        ...termOptions,
      }),
    )
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
      ;(await this.daemon).start()
      this.started = performance.now()
      this.setupHealthCheck()
    } else {
      ;(await this.daemon).stop()
      this.turnOffHealthCheck()

      this.setHealth({ result: "starting", message: null })
    }
  }

  private healthCheckCleanup: (() => null) | null = null
  private turnOffHealthCheck() {
    this.healthCheckCleanup?.()
  }
  private async setupHealthCheck() {
    if (this.healthCheckCleanup) return
    const trigger = (this.ready.trigger ?? defaultTrigger)(() => ({
      lastResult: this._health.result,
    }))

    const { promise: status, resolve: setStatus } = oncePromise<{
      done: true
    }>()
    new Promise(async () => {
      for (
        let res = await Promise.race([status, trigger.next()]);
        !res.done;
        res = await Promise.race([status, trigger.next()])
      ) {
        const handle = (await this.daemon).subContainerHandle

        if (handle) {
          const response: HealthCheckResult = await Promise.resolve(
            this.ready.fn(handle),
          ).catch((err) => {
            console.error(asError(err))
            return {
              result: "failure",
              message: "message" in err ? err.message : String(err),
            }
          })
          if (
            this.resolveReady &&
            (response.result === "success" || response.result === "disabled")
          ) {
            this.resolveReady()
          }
          await this.setHealth(response)
        } else {
          await this.setHealth({
            result: "failure",
            message: "Daemon not running",
          })
        }
      }
    }).catch((err) => console.error(`Daemon ${this.id} failed: ${err}`))

    this.healthCheckCleanup = () => {
      setStatus({ done: true })
      this.healthCheckCleanup = null
      return null
    }
  }

  onReady() {
    return this.readyPromise
  }

  private async setHealth(health: HealthCheckResult) {
    this._health = health
    this.healthWatchers.forEach((watcher) => watcher())
    const display = this.ready.display
    if (!display) {
      return
    }
    let result = health.result
    if (
      result === "failure" &&
      this.started &&
      performance.now() - this.started <= (this.ready.gracePeriod ?? 5000)
    )
      result = "starting"
    await this.effects.setHealth({
      ...health,
      id: this.id,
      name: display,
      result,
    } as SetHealth)
  }

  async updateStatus() {
    const healths = this.dependencies.map((d) => d.running && d._health)
    this.changeRunning(healths.every((x) => x && x.result === "success"))
  }
}
