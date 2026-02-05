import { Trigger } from "."
import { lastStatus } from "./lastStatus"

/**
 * Creates a trigger with different timing for success vs failure/starting states.
 *
 * This is a simplified wrapper around `lastStatus` for the common case
 * where you want one timing during healthy operation and another during
 * any error condition (failure or starting).
 *
 * @param o.duringSuccess - Trigger to use when the last check succeeded
 * @param o.duringError - Trigger to use for failure, starting, or unknown states
 * @returns A composite trigger that adapts to success/failure state
 *
 * @example
 * ```typescript
 * // Check every minute when healthy, every 5 seconds when unhealthy
 * const trigger = successFailure({
 *   duringSuccess: cooldownTrigger(60000),  // 1 minute
 *   duringError: cooldownTrigger(5000)      // 5 seconds
 * })
 * ```
 */
export const successFailure = (o: {
  duringSuccess: Trigger
  duringError: Trigger
}) => lastStatus({ success: o.duringSuccess, default: o.duringError })
