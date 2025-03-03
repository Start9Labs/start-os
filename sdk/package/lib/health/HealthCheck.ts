import { Effects, HealthCheckId, HealthReceipt } from "../../../base/lib/types"
import { HealthCheckResult } from "./checkFns/HealthCheckResult"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once, asError } from "../util"
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

export function healthCheck(o: HealthCheckParams) {
  new Promise(async () => {
    const start = performance.now()
    let currentValue: TriggerInput = {}
    const getCurrentValue = () => currentValue
    const gracePeriod = o.gracePeriod ?? 5000
    const trigger = (o.trigger ?? defaultTrigger)(getCurrentValue)
    const triggerFirstSuccess = once(() =>
      Promise.resolve(
        "onFirstSuccess" in o && o.onFirstSuccess
          ? o.onFirstSuccess()
          : undefined,
      ),
    )
    for (
      let res = await trigger.next();
      !res.done;
      res = await trigger.next()
    ) {
      try {
        let { result, message } = await o.fn()
        if (result === "failure" && performance.now() - start <= gracePeriod)
          result = "starting"
        await o.effects.setHealth({
          name: o.name,
          id: o.id,
          result,
          message: message || "",
        })
        currentValue.lastResult = result
        await triggerFirstSuccess().catch((err) => {
          console.error(asError(err))
        })
      } catch (e) {
        await o.effects.setHealth({
          name: o.name,
          id: o.id,
          result:
            performance.now() - start <= gracePeriod ? "starting" : "failure",
          message: asMessage(e) || "",
        })
        currentValue.lastResult = "failure"
      }
    }
  })
  return {} as HealthReceipt
}
function asMessage(e: unknown) {
  if (object({ message: unknown }).test(e)) return String(e.message)
  const value = String(e)
  if (value.length == null) return null
  return value
}
