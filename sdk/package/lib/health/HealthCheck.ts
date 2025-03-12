import { Effects, HealthCheckId } from "../../../base/lib/types"
import { HealthCheckResult } from "./checkFns/HealthCheckResult"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once, asError, Drop } from "../util"
import { object, unknown } from "ts-matches"

export type HealthCheckParams = {
  effects: Effects
  id: HealthCheckId
  name: string
  trigger?: Trigger
  gracePeriod?: number
  fn(): Promise<HealthCheckResult> | HealthCheckResult
  onFirstSuccess?: () => unknown | Promise<unknown>
}

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
  private constructor(o: HealthCheckParams) {
    super()
    this.promise = Promise.resolve().then(async () => {
      const getCurrentValue = () => this.currentValue
      const gracePeriod = o.gracePeriod ?? 5000
      const trigger = (o.trigger ?? defaultTrigger)(getCurrentValue)
      const triggerFirstSuccess = once(() =>
        Promise.resolve(
          "onFirstSuccess" in o && o.onFirstSuccess
            ? o.onFirstSuccess()
            : undefined,
        ),
      )
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
              await o.effects.setHealth({
                name: o.name,
                id: o.id,
                result,
                message: message || "",
              })
              this.currentValue.lastResult = result
              await triggerFirstSuccess().catch((err) => {
                console.error(asError(err))
              })
            } catch (e) {
              await o.effects.setHealth({
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
  static of(options: HealthCheckParams): HealthCheck {
    return new HealthCheck(options)
  }
  start() {
    if (this.started) return
    this.setStarted(performance.now())
  }
  stop() {
    if (!this.started) return
    this.setStarted(null)
  }
  onDrop(): void {
    this.exit()
  }
}

function asMessage(e: unknown) {
  if (object({ message: unknown }).test(e)) return String(e.message)
  const value = String(e)
  if (value.length == null) return null
  return value
}
