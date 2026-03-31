import { Effects, HealthCheckId } from '../../../base/lib/types'
import { HealthCheckResult } from './checkFns/HealthCheckResult'
import { Trigger } from '../trigger'
import { TriggerInput } from '../trigger/TriggerInput'
import { defaultTrigger } from '../trigger/defaultTrigger'
import { once, asError, Drop } from '../util'

/** Parameters for creating a health check */
export type HealthCheckParams = {
  id: HealthCheckId
  name: string
  trigger?: Trigger
  gracePeriod?: number
  fn(): Promise<HealthCheckResult> | HealthCheckResult
}

/**
 * A periodic health check that reports daemon readiness to the StartOS UI.
 *
 * Polls at an interval controlled by a {@link Trigger}, reporting results as
 * "starting" (during the grace period), "success", or "failure". Automatically
 * pauses when the daemon is stopped and resumes when restarted.
 */
export class HealthCheck extends Drop {
  private started: number | null = null
  private setStarted = (started: number | null) => {
    this.started = started
  }
  private exited = false
  private exit = () => {
    this.exited = true
  }
  private currentValue: TriggerInput = {}
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
                result === 'failure' &&
                performance.now() - started <= gracePeriod
              )
                result = 'starting'
              await effects.setHealth({
                name: o.name,
                id: o.id,
                result,
                message: message || '',
              })
              this.currentValue.lastResult = result
            } catch (e) {
              await effects.setHealth({
                name: o.name,
                id: o.id,
                result:
                  performance.now() - started <= gracePeriod
                    ? 'starting'
                    : 'failure',
                message: asMessage(e) || '',
              })
              this.currentValue.lastResult = 'failure'
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
   * Create a new HealthCheck instance and begin its polling loop.
   * @param effects - The effects context for reporting health status
   * @param options - Health check configuration (ID, name, check function, trigger, grace period)
   * @returns A new HealthCheck instance
   */
  static of(effects: Effects, options: HealthCheckParams): HealthCheck {
    return new HealthCheck(effects, options)
  }
  /** Signal that the daemon is running, enabling health check polling */
  start() {
    if (this.started) return
    this.setStarted(performance.now())
  }
  /** Signal that the daemon has stopped, pausing health check polling */
  stop() {
    if (!this.started) return
    this.setStarted(null)
  }
  onDrop(): void {
    this.exit()
  }
}

function asMessage(e: unknown) {
  if (typeof e === 'object' && e !== null && 'message' in e)
    return String((e as any).message)
  const value = String(e)
  if (value.length == null) return null
  return value
}
