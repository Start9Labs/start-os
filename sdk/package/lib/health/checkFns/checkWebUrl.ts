import { Effects } from "../../../../base/lib/types"
import { asError } from "../../util"
import { HealthCheckResult } from "./HealthCheckResult"
import { timeoutPromise } from "./index"
import "isomorphic-fetch"

/**
 * Checks if a web URL is reachable by making an HTTP request.
 *
 * This is useful for services that expose an HTTP health endpoint
 * or for checking if a web UI is responding.
 *
 * Note: This only checks if the request completes without network errors.
 * It does NOT check the HTTP status code - a 500 error response would
 * still be considered "success" since the server responded.
 *
 * @param effects - Effects instance (currently unused but included for API consistency)
 * @param url - The URL to fetch (e.g., "http://localhost:8080/health")
 * @param options.timeout - Maximum time to wait for a response in milliseconds (default: 1000)
 * @param options.successMessage - Message to include when check succeeds
 * @param options.errorMessage - Message to include when check fails
 * @returns Promise resolving to a HealthCheckResult
 *
 * @example
 * ```typescript
 * // Basic usage
 * const check = () => checkWebUrl(effects, 'http://localhost:8080/health')
 *
 * // With custom options
 * const check = () => checkWebUrl(effects, 'http://localhost:3000', {
 *   timeout: 5000,
 *   successMessage: 'Web UI is responding',
 *   errorMessage: 'Cannot reach web UI'
 * })
 *
 * // Use in health check config
 * daemons.addHealthCheck({
 *   id: 'web',
 *   name: 'Web Interface',
 *   fn: () => checkWebUrl(effects, 'http://localhost:8080')
 * })
 * ```
 */
export const checkWebUrl = async (
  effects: Effects,
  url: string,
  {
    timeout = 1000,
    successMessage = `Reached ${url}`,
    errorMessage = `Error while fetching URL: ${url}`,
  } = {},
): Promise<HealthCheckResult> => {
  return Promise.race([fetch(url), timeoutPromise(timeout)])
    .then(
      (x) =>
        ({
          result: "success",
          message: successMessage,
        }) as const,
    )
    .catch((e) => {
      console.warn(`Error while fetching URL: ${url}`)
      console.error(JSON.stringify(e))
      console.error(asError(e))
      return { result: "failure" as const, message: errorMessage }
    })
}
