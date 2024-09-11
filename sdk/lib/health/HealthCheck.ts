import { Effects } from "../Effects"
import { HealthCheckResult } from "./checkFns/HealthCheckResult"
import { HealthReceipt } from "./HealthReceipt"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once } from "../util/once"
import { object, unknown } from "ts-matches"
import { asError } from "../util/asError"

export type HealthCheckParams = {
  effects: Effects
  name: string
  trigger?: Trigger
  fn(): Promise<HealthCheckResult> | HealthCheckResult
  onFirstSuccess?: () => unknown | Promise<unknown>
}

export function healthCheck(o: HealthCheckParams) {
  new Promise(async () => {
    let currentValue: TriggerInput = {}
    const getCurrentValue = () => currentValue
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
        const { result, message } = await o.fn()
        await o.effects.setHealth({
          name: o.name,
          id: o.name,
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
          id: o.name,
          result: "failure",
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
