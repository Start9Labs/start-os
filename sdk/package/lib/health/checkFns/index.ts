/**
 * @module checkFns
 *
 * Provides pre-built health check functions for common use cases.
 * These can be used directly in health check configurations or as building blocks
 * for custom health checks.
 *
 * Available functions:
 * - `checkPortListening` - Check if a port is open and listening
 * - `checkWebUrl` - Check if a web URL is reachable
 * - `runHealthScript` - Run a shell script and check its exit code
 * - `timeoutPromise` - Helper to add timeouts to async operations
 *
 * @example
 * ```typescript
 * import { checkPortListening, checkWebUrl } from '@start9labs/sdk'
 *
 * // Check if port 8080 is listening
 * const portCheck = () => checkPortListening(effects, 8080, {
 *   successMessage: 'Server is listening',
 *   errorMessage: 'Server not responding'
 * })
 *
 * // Check if web UI is reachable
 * const webCheck = () => checkWebUrl(effects, 'http://localhost:8080/health', {
 *   timeout: 5000,
 *   successMessage: 'Web UI is up'
 * })
 * ```
 */
import { runHealthScript } from "./runHealthScript"
export { checkPortListening } from "./checkPortListening"
export { HealthCheckResult } from "./HealthCheckResult"
export { checkWebUrl } from "./checkWebUrl"

/**
 * Creates a promise that rejects after the specified timeout.
 * Useful for adding timeouts to async operations using `Promise.race()`.
 *
 * @param ms - Timeout duration in milliseconds
 * @param options.message - Error message when timeout occurs
 * @returns A promise that rejects after `ms` milliseconds
 *
 * @example
 * ```typescript
 * // Add a 5-second timeout to an async operation
 * const result = await Promise.race([
 *   someAsyncOperation(),
 *   timeoutPromise(5000, { message: 'Operation timed out' })
 * ])
 * ```
 */
export function timeoutPromise(ms: number, { message = "Timed out" } = {}) {
  return new Promise<never>((resolve, reject) =>
    setTimeout(() => reject(new Error(message)), ms),
  )
}
export { runHealthScript }
