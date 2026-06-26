import { runHealthScript } from './runHealthScript'
export { checkPortListening } from './checkPortListening'
export { HealthCheckResult } from './HealthCheckResult'
export { checkWebUrl } from './checkWebUrl'

/**
 * Create a promise that rejects after the specified timeout.
 * Useful for racing against long-running health checks.
 *
 * @param ms - Timeout duration in milliseconds
 * @param options.message - Custom error message (defaults to "Timed out")
 * @returns A promise that never resolves, only rejects after the timeout
 */
export function timeoutPromise(ms: number, { message = 'Timed out' } = {}) {
  return new Promise<never>((resolve, reject) =>
    setTimeout(() => reject(new Error(message)), ms),
  )
}
export { runHealthScript }
