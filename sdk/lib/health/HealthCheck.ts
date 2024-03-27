import { InterfaceReceipt } from "../interfaces/interfaceReceipt"
import { Daemon, Effects } from "../types"
import { CheckResult } from "./checkFns/CheckResult"
import { HealthReceipt } from "./HealthReceipt"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once } from "../util/once"
import { Overlay } from "../util/Overlay"

export function healthCheck(o: {
  effects: Effects
  name: string
  imageId: string
  trigger?: Trigger
  fn(overlay: Overlay): Promise<CheckResult> | CheckResult
  onFirstSuccess?: () => unknown | Promise<unknown>
}) {
  new Promise(async () => {
    const overlay = await Overlay.of(o.effects, o.imageId)
    try {
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
          const { status, message } = await o.fn(overlay)
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
            status: "failure",
            message: asMessage(e),
          })
          currentValue.lastResult = "failure"
        }
      }
    } finally {
      await overlay.destroy()
    }
  })
  return {} as HealthReceipt
}
function asMessage(e: unknown) {
  if (typeof e === "object" && e != null && "message" in e)
    return String(e.message)
  const value = String(e)
  if (value.length == null) return null
  return value
}
