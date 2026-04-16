import { HealthStatus } from '../../../base/lib/types'
import { Trigger } from '.'

type StatusIntervals = { [K in HealthStatus]?: number }

/**
 * A trigger with per-status polling intervals.
 *
 * Map each health check result to a cooldown duration in milliseconds.
 * Any status without an explicit interval uses the default (30 s if omitted).
 *
 * @param defaultMs - Fallback interval (ms) for any status not listed. Defaults to 30 s.
 * @param intervals - Per-status overrides
 *
 * @example
 * ```ts
 * trigger: sdk.trigger.statusTrigger(30_000, {
 *   starting: 5_000,
 *   failure: 5_000,
 * })
 * ```
 */
export function statusTrigger(
  defaultMs: number,
  intervals: StatusIntervals = {},
): Trigger {
  return async function* (getInput) {
    while (true) {
      const { lastResult } = getInput()
      const ms = lastResult
        ? (intervals[lastResult] ?? defaultMs)
        : (intervals.starting ?? defaultMs)
      await new Promise((resolve) => setTimeout(resolve, ms))
      yield
    }
  }
}
