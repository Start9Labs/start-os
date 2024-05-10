import { InterfaceReceipt } from "../interfaces/interfaceReceipt"
import { Daemon, Effects, SDKManifest } from "../types"
import { CheckResult } from "./checkFns/CheckResult"
import { HealthReceipt } from "./HealthReceipt"
import { Trigger } from "../trigger"
import { TriggerInput } from "../trigger/TriggerInput"
import { defaultTrigger } from "../trigger/defaultTrigger"
import { once } from "../util/once"
import { Overlay } from "../util/Overlay"
import { object, unknown } from "ts-matches"

export type HealthCheckParams<Manifest extends SDKManifest> = {
  effects: Effects
  name: string
  image: {
    id: Manifest["images"][number]
    sharedRun?: boolean
  }
  trigger?: Trigger
  fn(overlay: Overlay): Promise<CheckResult> | CheckResult
  onFirstSuccess?: () => unknown | Promise<unknown>
}

export function healthCheck<Manifest extends SDKManifest>(
  o: HealthCheckParams<Manifest>,
) {
  new Promise(async () => {
    const overlay = await Overlay.of(o.effects, o.image)
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
            id: o.name,
            result: status,
            message: message || "",
          })
          currentValue.hadSuccess = true
          currentValue.lastResult = "success"
          await triggerFirstSuccess().catch((err) => {
            console.error(err)
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
    } finally {
      await overlay.destroy()
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
