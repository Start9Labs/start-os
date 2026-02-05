import { Trigger } from "."
import { HealthStatus } from "../../../base/lib/types"

/**
 * Configuration for status-based trigger selection.
 * Maps health statuses to triggers, with a required default fallback.
 */
export type LastStatusTriggerParams = { [k in HealthStatus]?: Trigger } & {
  /** Trigger to use when no status-specific trigger is defined */
  default: Trigger
}

/**
 * Creates a trigger that dynamically selects timing based on the last health check result.
 *
 * This allows different check frequencies for each health status:
 * - `success` - When the service is healthy
 * - `failure` - When the service is unhealthy
 * - `starting` - When the service is still starting up
 *
 * Uses the `default` trigger for any status not explicitly configured.
 *
 * @param o - Map of health statuses to triggers, plus a required default
 * @returns A composite trigger that adapts to the current health status
 *
 * @example
 * ```typescript
 * // Check frequently during failure, rarely during success
 * const adaptiveTrigger = lastStatus({
 *   success: cooldownTrigger(60000),   // 60s when healthy
 *   failure: cooldownTrigger(5000),    // 5s when unhealthy
 *   starting: cooldownTrigger(2000),   // 2s while starting
 *   default: cooldownTrigger(10000)    // 10s fallback
 * })
 * ```
 */
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
