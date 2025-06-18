import { Trigger } from "."
import { HealthStatus } from "../../../base/lib/types"

export type LastStatusTriggerParams = { [k in HealthStatus]?: Trigger } & {
  default: Trigger
}

export function lastStatus(o: LastStatusTriggerParams): Trigger {
  return async function* (getInput) {
    let trigger = o.default(getInput)
    const triggers: {
      [k in HealthStatus]?: AsyncIterator<unknown, unknown, never>
    } & { default: AsyncIterator<unknown, unknown, never> } = {
      default: trigger,
    }
    while (true) {
      let currentValue = getInput()
      let prev: HealthStatus | "default" | undefined = currentValue.lastResult
      if (!prev) {
        yield
        continue
      }
      if (!(prev in o)) {
        prev = "default"
      }
      if (!triggers[prev]) {
        triggers[prev] = o[prev]!(getInput)
      }
      await triggers[prev]?.next()
      yield
    }
  }
}
