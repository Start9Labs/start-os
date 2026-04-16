/**
 * A trigger that polls at a fixed interval.
 *
 * @param timeMs - Milliseconds to wait between each health check invocation
 *
 * @example
 * ```ts
 * // Poll every 30 seconds
 * ready: {
 *   display: 'My Check',
 *   trigger: sdk.trigger.cooldownTrigger(30_000),
 *   fn: async () => { ... },
 * }
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
