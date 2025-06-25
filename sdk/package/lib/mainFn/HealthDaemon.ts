import { HealthCheckResult } from "../health/checkFns"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { Ready } from "./Daemons"
import { Daemon } from "./Daemon"
import { SetHealth, Effects, SDKManifest } from "../../../base/lib/types"
import { DEFAULT_SIGTERM_TIMEOUT } from "."
import { asError } from "../../../base/lib/util/asError"
import { Oneshot } from "./Oneshot"
import { SubContainer } from "../util/SubContainer"

const oncePromise = <T>() => {
  let resolve: (value: T) => void
  const promise = new Promise<T>((res) => {
    resolve = res
  })
  return { resolve: resolve!, promise }
}

export const EXIT_SUCCESS = "EXIT_SUCCESS" as const

/**
 * Wanted a structure that deals with controlling daemons by their health status
 * States:
 * -- Waiting for dependencies to be success
 * -- Running: Daemon is running and the status is in the health
 *
 */
export class HealthDaemon<Manifest extends SDKManifest> {
  private _health: HealthCheckResult = { result: "starting", message: null }
  private healthWatchers: Array<() => unknown> = []
  private running = false
  private started?: number
  private resolveReady: (() => void) | undefined
  private resolvedReady: boolean = false
  private readyPromise: Promise<void>
  constructor(
    private readonly daemon: Promise<Daemon<Manifest>> | null,
    private readonly dependencies: HealthDaemon<Manifest>[],
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
  }) {
    this.healthWatchers = []
    this.running = false
    this.healthCheckCleanup?.()

    await this.daemon?.then((d) =>
      d.term({
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
      console.debug(`Launching ${this.id}...`)
      this.setupHealthCheck()
      ;(await this.daemon)?.start()
      this.started = performance.now()
    } else {
      console.debug(`Stopping ${this.id}...`)
      ;(await this.daemon)?.stop()
      this.turnOffHealthCheck()

      this.setHealth({ result: "starting", message: null })
    }
  }

  private healthCheckCleanup: (() => null) | null = null
  private turnOffHealthCheck() {
    this.healthCheckCleanup?.()

    this.resolvedReady = false
    this.readyPromise = new Promise(
      (resolve) =>
        (this.resolveReady = () => {
          resolve()
          this.resolvedReady = true
        }),
    )
  }
  private async setupHealthCheck() {
    const daemon = await this.daemon
    daemon?.onExit((success) => {
      if (success && this.ready === "EXIT_SUCCESS") {
        this.setHealth({ result: "success", message: null })
      } else if (!success) {
        this.setHealth({
          result: "failure",
          message: `${this.id} daemon crashed`,
        })
      } else if (!daemon.isOneshot()) {
        this.setHealth({
          result: "failure",
          message: `${this.id} daemon exited`,
        })
      }
    })
    if (this.ready === "EXIT_SUCCESS") return
    if (this.healthCheckCleanup) return
    const trigger = (this.ready.trigger ?? defaultTrigger)(() => ({
      lastResult: this._health.result,
    }))

    const { promise: status, resolve: setStatus } = oncePromise<{
      done: true
    }>()
    new Promise(async () => {
      if (this.ready === "EXIT_SUCCESS") return
      for (
        let res = await Promise.race([status, trigger.next()]);
        !res.done;
        res = await Promise.race([status, trigger.next()])
      ) {
        const response: HealthCheckResult = await Promise.resolve(
          this.ready.fn(),
        ).catch((err) => {
          console.error(asError(err))
          return {
            result: "failure",
            message: "message" in err ? err.message : String(err),
          }
        })

        await this.setHealth(response)
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

  get isReady() {
    return this.resolvedReady
  }

  private async setHealth(health: HealthCheckResult) {
    const changed = this._health.result !== health.result
    this._health = health
    if (this.resolveReady && health.result === "success") {
      this.resolveReady()
    }
    if (changed) this.healthWatchers.forEach((watcher) => watcher())
    if (this.ready === "EXIT_SUCCESS") return
    const display = this.ready.display
    if (!display) {
      return
    }
    let result = health.result
    if (
      result === "failure" &&
      this.started &&
      performance.now() - this.started <= (this.ready.gracePeriod ?? 10_000)
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
    const healths = this.dependencies.map((d) => ({
      health: d.running && d._health,
      id: d.id,
    }))
    const waitingOn = healths.filter(
      (h) => !h.health || h.health.result !== "success",
    )
    if (waitingOn.length)
      console.debug(
        `daemon ${this.id} waiting on ${waitingOn.map((w) => w.id)}`,
      )
    this.changeRunning(!waitingOn.length)
  }

  async init() {
    if (this.ready !== "EXIT_SUCCESS" && this.ready.display) {
      this.effects.setHealth({
        id: this.id,
        message: null,
        name: this.ready.display,
        result: "starting",
      })
    }
    await this.updateStatus()
  }
}
