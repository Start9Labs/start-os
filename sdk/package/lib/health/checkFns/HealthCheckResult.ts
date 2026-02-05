import { T } from "../../../../base/lib"

/**
 * The result returned by a health check function.
 *
 * Contains the status result and a message describing the current state.
 * The `name` field is added automatically by the health check system.
 *
 * @example
 * ```typescript
 * // Success result
 * const healthy: HealthCheckResult = {
 *   result: 'success',
 *   message: 'Server responding on port 8080'
 * }
 *
 * // Failure result
 * const unhealthy: HealthCheckResult = {
 *   result: 'failure',
 *   message: 'Connection refused on port 8080'
 * }
 *
 * // Starting result (usually set automatically by grace period)
 * const starting: HealthCheckResult = {
 *   result: 'starting',
 *   message: 'Waiting for server to initialize...'
 * }
 * ```
 */
export type HealthCheckResult = Omit<T.NamedHealthCheckResult, "name">
