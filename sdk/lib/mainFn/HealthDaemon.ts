import { HealthCheckResult } from "../health/checkFns"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { Ready } from "./Daemons"
import { Daemon } from "./Daemon"
import { Effects, SetHealth } from "../types"
import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { asError } from "../util/asError"

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
    this.updateStatus()
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
      this.setupHealthCheck()
    } else {
      ;(await this.daemon).stop()
      this.turnOffHealthCheck()

      this.setHealth({ result: "starting", message: null })
    }
  }

  private healthCheckCleanup: (() => void) | null = null
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
    }
  }

  private async setHealth(health: HealthCheckResult) {
    this._health = health
    this.healthWatchers.forEach((watcher) => watcher())
    const display = this.ready.display
    const result = health.result
    if (!display) {
      return
    }
    await this.effects.setHealth({
      ...health,
      id: this.id,
      name: display,
    } as SetHealth)
  }

  private async updateStatus() {
    const healths = this.dependencies.map((d) => d._health)
    this.changeRunning(healths.every((x) => x.result === "success"))
  }
}
