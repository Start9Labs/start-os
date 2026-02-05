import { HealthCheckResult } from "./HealthCheckResult"
import { timeoutPromise } from "./index"
import { SubContainer } from "../../util/SubContainer"
import { SDKManifest } from "../../types"

/**
 * Runs a command inside a subcontainer and uses the exit code for health status.
 *
 * This is useful when the service provides a CLI health check command or when
 * you want to run a custom bash script to determine health status. The command
 * must exit with code 0 for success; any other exit code is treated as failure.
 *
 * @typeParam Manifest - The service manifest type
 * @param runCommand - Command and arguments to execute (e.g., ['pg_isready', '-U', 'postgres'])
 * @param subcontainer - The SubContainer to run the command in
 * @param options.timeout - Maximum time to wait for the command in milliseconds (default: 30000)
 * @param options.errorMessage - Message to include when the command fails
 * @param options.message - Function to generate success message from stdout
 * @returns Promise resolving to a HealthCheckResult
 * @throws HealthCheckResult with result: "failure" if the command fails or times out
 *
 * @example
 * ```typescript
 * // Check PostgreSQL readiness using pg_isready
 * const check = () => runHealthScript(
 *   ['pg_isready', '-U', 'postgres'],
 *   subcontainer,
 *   {
 *     timeout: 5000,
 *     errorMessage: 'PostgreSQL is not ready'
 *   }
 * )
 *
 * // Custom bash health check
 * const check = () => runHealthScript(
 *   ['bash', '-c', 'curl -sf http://localhost:8080/health || exit 1'],
 *   subcontainer,
 *   { errorMessage: 'Health endpoint check failed' }
 * )
 *
 * // Use in health check config
 * daemons.addHealthCheck({
 *   id: 'cli',
 *   name: 'CLI Health Check',
 *   fn: () => runHealthScript(['myapp', 'healthcheck'], subcontainer)
 * })
 * ```
 */
export const runHealthScript = async <Manifest extends SDKManifest>(
  runCommand: string[],
  subcontainer: SubContainer<Manifest>,
  {
    timeout = 30000,
    errorMessage = `Error while running command: ${runCommand}`,
    message = (res: string) =>
      `Have ran script ${runCommand} and the result: ${res}`,
  } = {},
): Promise<HealthCheckResult> => {
  const res = await Promise.race([
    subcontainer.execFail(runCommand),
    timeoutPromise(timeout),
  ]).catch((e) => {
    console.warn(errorMessage)
    console.warn(JSON.stringify(e))
    console.warn(e.toString())
    throw { result: "failure", message: errorMessage } as HealthCheckResult
  })
  return {
    result: "success",
    message: message(res.stdout.toString()),
  } as HealthCheckResult
}
