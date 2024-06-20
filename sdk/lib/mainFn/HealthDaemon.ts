import { CheckResult } from "../health/checkFns"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { Ready } from "./Daemons"
import { Daemon } from "./Daemon"
import { Effects } from "../types"

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
  #health: CheckResult = { status: "starting", message: null }
  #healthWatchers: Array<() => unknown> = []
  #running = false
  #hadSuccess = false
  constructor(
    readonly daemon: Promise<Daemon>,
    readonly daemonIndex: number,
    readonly dependencies: HealthDaemon[],
    readonly id: string,
    readonly ids: string[],
    readonly ready: Ready,
    readonly effects: Effects,
  ) {
    this.updateStatus()
    this.dependencies.forEach((d) => d.addWatcher(() => this.updateStatus()))
  }

  /** Run after we want to do cleanup */
  async term(termOptions?: {
    signal?: NodeJS.Signals | undefined
    timeout?: number | undefined
  }) {
    this.#healthWatchers = []
    this.#running = false
    this.#healthCheckCleanup?.()

    await this.daemon.then((d) => d.stop(termOptions))
  }

  /** Want to add another notifier that the health might have changed */
  addWatcher(watcher: () => unknown) {
    this.#healthWatchers.push(watcher)
  }

  get health() {
    return Object.freeze(this.#health)
  }

  private async changeRunning(newStatus: boolean) {
    if (this.#running === newStatus) return

    this.#running = newStatus

    if (newStatus) {
      ;(await this.daemon).start()
      this.setupHealthCheck()
    } else {
      ;(await this.daemon).stop()
      this.turnOffHealthCheck()

      this.setHealth({ status: "starting", message: null })
    }
  }

  #healthCheckCleanup: (() => void) | null = null
  private turnOffHealthCheck() {
    this.#healthCheckCleanup?.()
  }
  private async setupHealthCheck() {
    if (this.#healthCheckCleanup) return
    const trigger = (this.ready.trigger ?? defaultTrigger)(() => ({
      hadSuccess: this.#hadSuccess,
      lastResult: this.#health.status,
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
        const response: CheckResult = await Promise.resolve(
          this.ready.fn(),
        ).catch((err) => {
          console.error(err)
          return {
            status: "failure",
            message: "message" in err ? err.message : String(err),
          }
        })
        this.setHealth(response)
        if (response.status === "success") {
          this.#hadSuccess = true
        }
      }
    }).catch((err) => console.error(`Daemon ${this.id} failed: ${err}`))

    this.#healthCheckCleanup = () => {
      setStatus({ done: true })
      this.#healthCheckCleanup = null
    }
  }

  private setHealth(health: CheckResult) {
    this.#health = health
    this.#healthWatchers.forEach((watcher) => watcher())
    const display = this.ready.display
    const status = health.status
    if (!display) {
      return
    }
    if (
      status === "success" ||
      status === "disabled" ||
      status === "starting"
    ) {
      this.effects.setHealth({
        result: status,
        message: health.message,
        id: this.id,
        name: display,
      })
    } else {
      this.effects.setHealth({
        result: health.status,
        message: health.message || "",
        id: this.id,
        name: display,
      })
    }
  }

  private async updateStatus() {
    const healths = this.dependencies.map((d) => d.#health)
    this.changeRunning(healths.every((x) => x.status === "success"))
  }
}
