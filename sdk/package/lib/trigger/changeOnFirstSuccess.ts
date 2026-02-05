import { Trigger } from "./index"

/**
 * Creates a trigger that uses different timing before and after the first successful health check.
 *
 * This is useful for services that need frequent checks during startup (to quickly report
 * when they become healthy) but can reduce check frequency once they're running stably.
 *
 * The trigger switches permanently to `afterFirstSuccess` timing once a success is seen.
 * It does NOT switch back even if the service later becomes unhealthy.
 *
 * @param o.beforeFirstSuccess - Trigger to use until the first successful health check
 * @param o.afterFirstSuccess - Trigger to use after the first successful health check
 * @returns A composite trigger that switches behavior after first success
 *
 * @example
 * ```typescript
 * // Check every second while starting, every 30 seconds once healthy
 * const trigger = changeOnFirstSuccess({
 *   beforeFirstSuccess: cooldownTrigger(1000),   // 1s while starting
 *   afterFirstSuccess: cooldownTrigger(30000)    // 30s after healthy
 * })
 *
 * // Use in a health check
 * daemons.addHealthCheck({
 *   id: 'main',
 *   name: 'Main Health',
 *   trigger,
 *   fn: checkServiceHealth
 * })
 * ```
 */
export function changeOnFirstSuccess(o: {
  beforeFirstSuccess: Trigger
  afterFirstSuccess: Trigger
}): Trigger {
  return async function* (getInput) {
    let currentValue = getInput()
    while (!currentValue.lastResult) {
      yield
      currentValue = getInput()
    }
    const beforeFirstSuccess = o.beforeFirstSuccess(getInput)
    for (
      let res = await beforeFirstSuccess.next();
      currentValue?.lastResult !== "success" && !res.done;
      res = await beforeFirstSuccess.next()
    ) {
      yield
      currentValue = getInput()
    }
    const afterFirstSuccess = o.afterFirstSuccess(getInput)
    for (
      let res = await afterFirstSuccess.next();
      !res.done;
      res = await afterFirstSuccess.next()
    ) {
      yield
      currentValue = getInput()
    }
  }
}
