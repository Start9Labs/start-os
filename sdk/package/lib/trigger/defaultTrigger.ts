import { cooldownTrigger } from "./cooldownTrigger"
import { changeOnFirstSuccess } from "./changeOnFirstSuccess"

/**
 * The default trigger used when no custom trigger is specified for a health check.
 *
 * Provides sensible defaults for most services:
 * - **Before first success**: Checks every 1 second (rapid during startup)
 * - **After first success**: Checks every 30 seconds (stable once healthy)
 *
 * This trigger is automatically used by `Daemons.addDaemon()` and `Daemons.addHealthCheck()`
 * when no `trigger` option is provided.
 *
 * @example
 * ```typescript
 * // These are equivalent - both use defaultTrigger
 * daemons.addHealthCheck({
 *   id: 'main',
 *   name: 'Main',
 *   fn: checkHealth
 *   // trigger: defaultTrigger  // implicit
 * })
 *
 * // Custom trigger overrides the default
 * daemons.addHealthCheck({
 *   id: 'main',
 *   name: 'Main',
 *   trigger: cooldownTrigger(5000),  // Check every 5s instead
 *   fn: checkHealth
 * })
 * ```
 */
export const defaultTrigger = changeOnFirstSuccess({
  beforeFirstSuccess: cooldownTrigger(1000),
  afterFirstSuccess: cooldownTrigger(30000),
})
