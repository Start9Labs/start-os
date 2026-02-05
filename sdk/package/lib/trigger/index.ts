/**
 * @module trigger
 *
 * Triggers control when health checks are executed. They are async generators
 * that yield when a health check should run. This allows fine-grained control
 * over check frequency based on the service's current state.
 *
 * Built-in triggers:
 * - `cooldownTrigger(ms)` - Simple timer-based trigger
 * - `changeOnFirstSuccess` - Different timing before/after first successful check
 * - `successFailure` - Different timing based on success/failure state
 * - `lastStatus` - Configurable timing per health status
 *
 * @example
 * ```typescript
 * // Check every 5 seconds
 * const trigger = cooldownTrigger(5000)
 *
 * // Fast checks until healthy, then slow down
 * const adaptiveTrigger = changeOnFirstSuccess({
 *   beforeFirstSuccess: cooldownTrigger(1000),  // 1s while starting
 *   afterFirstSuccess: cooldownTrigger(30000)   // 30s once healthy
 * })
 * ```
 */
import { TriggerInput } from "./TriggerInput"
export { changeOnFirstSuccess } from "./changeOnFirstSuccess"
export { cooldownTrigger } from "./cooldownTrigger"

/**
 * A trigger function that controls when health checks execute.
 *
 * Triggers are async generator factories. Given a function to get the current
 * input state (e.g., last health result), they return an async iterator that
 * yields when a health check should run.
 *
 * @param getInput - Function returning the current trigger input state
 * @returns An async iterator that yields when a check should run
 *
 * @example
 * ```typescript
 * // Custom trigger that checks every 10s during success, 2s during failure
 * const myTrigger: Trigger = (getInput) => {
 *   return (async function* () {
 *     while (true) {
 *       const { lastResult } = getInput()
 *       const delay = lastResult === 'success' ? 10000 : 2000
 *       await new Promise(r => setTimeout(r, delay))
 *       yield
 *     }
 *   })()
 * }
 * ```
 */
export type Trigger = (
  getInput: () => TriggerInput,
) => AsyncIterator<unknown, unknown, never>
