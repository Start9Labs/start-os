/**
 * Creates a simple timer-based trigger that fires at regular intervals.
 * This is the most basic trigger type - it just waits the specified
 * time between each health check.
 *
 * @param timeMs - Interval between health checks in milliseconds
 * @returns A trigger factory function
 *
 * @example
 * ```typescript
 * // Check health every 5 seconds
 * const trigger = cooldownTrigger(5000)
 *
 * // Use in a health check
 * daemons.addHealthCheck({
 *   id: 'main',
 *   name: 'Main Check',
 *   trigger: cooldownTrigger(10000),  // Every 10 seconds
 *   fn: async () => ({ result: 'success', message: 'OK' })
 * })
 * ```
 */
export function cooldownTrigger(timeMs: number) {
  return async function* () {
    while (true) {
      await new Promise((resolve) => setTimeout(resolve, timeMs))
      yield
    }
  }
}
