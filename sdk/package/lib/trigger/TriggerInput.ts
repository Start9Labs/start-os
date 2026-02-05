import { HealthStatus } from "../../../base/lib/types"

/**
 * Input state provided to trigger functions.
 * Contains information about the health check's current state
 * that triggers can use to adjust their timing behavior.
 *
 * @example
 * ```typescript
 * const myTrigger: Trigger = (getInput) => {
 *   return (async function* () {
 *     while (true) {
 *       const input: TriggerInput = getInput()
 *       // Check more frequently if last result was failure
 *       const delay = input.lastResult === 'failure' ? 1000 : 30000
 *       await new Promise(r => setTimeout(r, delay))
 *       yield
 *     }
 *   })()
 * }
 * ```
 */
export type TriggerInput = {
  /** The result of the most recent health check execution, if any */
  lastResult?: HealthStatus
}
