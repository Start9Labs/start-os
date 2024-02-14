import { InterfaceReceipt } from "../interfaces/interfaceReceipt"
import { Daemon, Effects } from "../types"
import { CheckResult } from "./checkFns/CheckResult"
import { HealthReceipt } from "./HealthReceipt"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once } from "../util/once"

export function healthCheck(o: {
  effects: Effects
  name: string
  trigger?: Trigger
  fn(): Promise<CheckResult> | CheckResult
  onFirstSuccess?: () => unknown | Promise<unknown>
}) {
  new Promise(async () => {
    let currentValue: TriggerInput = {
      hadSuccess: false,
    }
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
        const { status, message } = await o.fn()
        await o.effects.setHealth({
          name: o.name,
          status,
          message,
        })
        currentValue.hadSuccess = true
        currentValue.lastResult = "passing"
        await triggerFirstSuccess().catch((err) => {
          console.error(err)
        })
      } catch (e) {
        await o.effects.setHealth({
          name: o.name,
          status: "failing",
          message: asMessage(e),
        })
        currentValue.lastResult = "failing"
      }
    }
  })
  return {} as HealthReceipt
}
function asMessage(e: unknown) {
  if (typeof e === "object" && e != null && "message" in e)
    return String(e.message)
  const value = String(e)
  if (value.length == null) return undefined
  return value
}
