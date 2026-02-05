/**
 * @module HealthCheck
 *
 * Provides the core health check management class that runs periodic health checks
 * and reports results to the StartOS host.
 */
import { Effects, HealthCheckId } from "../../../base/lib/types"
import { HealthCheckResult } from "./checkFns/HealthCheckResult"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once, asError, Drop } from "../util"
import { object, unknown } from "ts-matches"

/**
 * Configuration options for creating a health check.
 *
 * @example
 * ```typescript
 * const params: HealthCheckParams = {
 *   id: 'main',
 *   name: 'Main Service',
 *   gracePeriod: 30000,  // 30s grace period
 *   trigger: cooldownTrigger(5000),  // Check every 5s
 *   fn: async () => {
 *     const isHealthy = await checkService()
 *     return {
 *       result: isHealthy ? 'success' : 'failure',
 *       message: isHealthy ? 'Service running' : 'Service not responding'
 *     }
 *   }
 * }
 * ```
 */
export type HealthCheckParams = {
  /** Unique identifier for this health check (e.g., 'main', 'rpc', 'database') */
  id: HealthCheckId
  /** Human-readable name displayed in the StartOS UI */
  name: string
  /**
   * Trigger controlling when the health check runs.
   * @default defaultTrigger (1s before first success, 30s after)
   */
  trigger?: Trigger
  /**
   * Time in milliseconds during which failures are reported as "starting" instead.
   * This prevents false failure alerts during normal service startup.
   * @default 10000 (10 seconds)
   */
  gracePeriod?: number
  /**
   * The health check function. Called periodically according to the trigger.
   * Should return (or resolve to) a HealthCheckResult with result and message.
   */
  fn(): Promise<HealthCheckResult> | HealthCheckResult
}

/**
 * Manages periodic health check execution for a service.
 *
 * HealthCheck runs a check function according to a trigger schedule and reports
 * results to StartOS. It handles:
 * - Grace period logic (failures during startup report as "starting")
 * - Trigger-based scheduling (adjustable check frequency)
 * - Error handling (exceptions become failure results)
 * - Start/stop lifecycle management
 *
 * Usually created indirectly via `Daemons.addDaemon()` or `Daemons.addHealthCheck()`,
 * but can be created directly with `HealthCheck.of()` for advanced use cases.
 *
 * @example
 * ```typescript
 * // Direct creation (advanced usage)
 * const check = HealthCheck.of(effects, {
 *   id: 'database',
 *   name: 'Database Connection',
 *   gracePeriod: 20000,
 *   trigger: cooldownTrigger(10000),
 *   fn: async () => {
 *     const connected = await db.ping()
 *     return {
 *       result: connected ? 'success' : 'failure',
 *       message: connected ? 'Connected' : 'Cannot reach database'
 *     }
 *   }
 * })
 *
 * // Start checking (usually tied to daemon start)
 * check.start()
 *
 * // Stop checking (usually tied to daemon stop)
 * check.stop()
 * ```
 */
export class HealthCheck extends Drop {
  /** @internal Timestamp when the service was started (null if stopped) */
  private started: number | null = null
  /** @internal Callback to update started state and wake the check loop */
  private setStarted = (started: number | null) => {
    this.started = started
  }
  /** @internal Flag indicating the check loop should exit */
  private exited = false
  /** @internal Callback to signal the check loop to exit */
  private exit = () => {
    this.exited = true
  }
  /** @internal Current trigger input state */
  private currentValue: TriggerInput = {}
  /** @internal Promise representing the running check loop */
  private promise: Promise<void>
  private constructor(effects: Effects, o: HealthCheckParams) {
    super()
    this.promise = Promise.resolve().then(async () => {
      const getCurrentValue = () => this.currentValue
      const gracePeriod = o.gracePeriod ?? 10_000
      const trigger = (o.trigger ?? defaultTrigger)(getCurrentValue)
      const checkStarted = () =>
        [
          this.started,
          new Promise<void>((resolve) => {
            this.setStarted = (started: number | null) => {
              this.started = started
              resolve()
            }
            this.exit = () => {
              this.exited = true
              resolve()
            }
          }),
        ] as const
      let triggered = false
      while (!this.exited) {
        const [started, changed] = checkStarted()
        let race:
          | [Promise<void>]
          | [Promise<void>, Promise<IteratorResult<unknown, unknown>>] = [
          changed,
        ]
        if (started) {
          race = [...race, trigger.next()]
          if (triggered) {
            try {
              let { result, message } = await o.fn()
              if (
                result === "failure" &&
                performance.now() - started <= gracePeriod
              )
                result = "starting"
              await effects.setHealth({
                name: o.name,
                id: o.id,
                result,
                message: message || "",
              })
              this.currentValue.lastResult = result
            } catch (e) {
              await effects.setHealth({
                name: o.name,
                id: o.id,
                result:
                  performance.now() - started <= gracePeriod
                    ? "starting"
                    : "failure",
                message: asMessage(e) || "",
              })
              this.currentValue.lastResult = "failure"
            }
          }
        } else triggered = false
        const raced = await Promise.race(race)
        if (raced) {
          if (raced.done) break
          triggered = true
        }
      }
    })
  }
  /**
   * Creates a new HealthCheck instance.
   *
   * @param effects - Effects instance for communicating with StartOS
   * @param options - Health check configuration
   * @returns A new HealthCheck instance (initially stopped)
   *
   * @example
   * ```typescript
   * const check = HealthCheck.of(effects, {
   *   id: 'main',
   *   name: 'Main',
   *   fn: () => ({ result: 'success', message: 'OK' })
   * })
   * ```
   */
  static of(effects: Effects, options: HealthCheckParams): HealthCheck {
    return new HealthCheck(effects, options)
  }

  /**
   * Starts the health check loop.
   * The check function will begin executing according to the trigger schedule.
   * Has no effect if already started.
   */
  start() {
    if (this.started) return
    this.setStarted(performance.now())
  }

  /**
   * Stops the health check loop.
   * The check function will stop executing until `start()` is called again.
   * Has no effect if already stopped.
   */
  stop() {
    if (!this.started) return
    this.setStarted(null)
  }

  /**
   * Called when the HealthCheck is being disposed.
   * Signals the check loop to exit permanently.
   * @internal
   */
  onDrop(): void {
    this.exit()
  }
}

/**
 * Extracts an error message from an unknown error value.
 * @internal
 */
function asMessage(e: unknown) {
  if (object({ message: unknown }).test(e)) return String(e.message)
  const value = String(e)
  if (value.length == null) return null
  return value
}
