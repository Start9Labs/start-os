import { Effects } from "../../../../base/lib/types"
import { stringFromStdErrOut } from "../../util"
import { HealthCheckResult } from "./HealthCheckResult"
import { promisify } from "node:util"
import * as CP from "node:child_process"

const cpExec = promisify(CP.exec)

/**
 * Parses /proc/net/tcp* or /proc/net/udp* output to check if a port is listening.
 *
 * @param x - Raw content from /proc/net/tcp or similar file
 * @param port - Port number to look for
 * @param address - Optional specific address to match (undefined matches any)
 * @returns True if the port is found in the listening sockets
 * @internal
 */
export function containsAddress(x: string, port: number, address?: bigint) {
  const readPorts = x
    .split("\n")
    .filter(Boolean)
    .splice(1)
    .map((x) => x.split(" ").filter(Boolean)[1]?.split(":"))
    .filter((x) => x?.length > 1)
    .map(([addr, p]) => [BigInt(`0x${addr}`), Number.parseInt(p, 16)] as const)
  return !!readPorts.find(
    ([addr, p]) => (address === undefined || address === addr) && port === p,
  )
}

/**
 * Checks if a specific port is listening on the local system.
 *
 * This is a low-level health check that reads from /proc/net/ to determine
 * if a service is listening on a port. It checks both TCP and UDP, on both
 * IPv4 and IPv6 interfaces.
 *
 * This is useful for services where you want to verify the server process
 * has started and is accepting connections, even if it's not yet responding
 * to application-level requests.
 *
 * @param effects - Effects instance (currently unused but included for API consistency)
 * @param port - The port number to check
 * @param options.successMessage - Message to include when the port is listening
 * @param options.errorMessage - Message to include when the port is not listening
 * @param options.timeoutMessage - Message when the check times out (default: auto-generated)
 * @param options.timeout - Maximum time to wait for the check in milliseconds (default: 1000)
 * @returns Promise resolving to a HealthCheckResult
 *
 * @example
 * ```typescript
 * // Check if PostgreSQL is listening on port 5432
 * const check = () => checkPortListening(effects, 5432, {
 *   successMessage: 'PostgreSQL is accepting connections',
 *   errorMessage: 'PostgreSQL is not listening on port 5432'
 * })
 *
 * // Use in health check config
 * daemons.addHealthCheck({
 *   id: 'database',
 *   name: 'Database Port',
 *   fn: () => checkPortListening(effects, 5432, {
 *     successMessage: 'Database listening',
 *     errorMessage: 'Database not responding'
 *   })
 * })
 * ```
 */
export async function checkPortListening(
  effects: Effects,
  port: number,
  options: {
    errorMessage: string
    successMessage: string
    timeoutMessage?: string
    timeout?: number
  },
): Promise<HealthCheckResult> {
  return Promise.race<HealthCheckResult>([
    Promise.resolve().then(async () => {
      const hasAddress =
        containsAddress(
          await cpExec(`cat /proc/net/tcp`, {}).then(stringFromStdErrOut),
          port,
        ) ||
        containsAddress(
          await cpExec(`cat /proc/net/tcp6`, {}).then(stringFromStdErrOut),
          port,
          BigInt(0),
        ) ||
        containsAddress(
          await cpExec("cat /proc/net/udp", {}).then(stringFromStdErrOut),
          port,
        ) ||
        containsAddress(
          await cpExec("cat /proc/net/udp6", {}).then(stringFromStdErrOut),
          port,
          BigInt(0),
        )
      if (hasAddress) {
        return { result: "success", message: options.successMessage }
      }
      return {
        result: "failure",
        message: options.errorMessage,
      }
    }),
    new Promise((resolve) => {
      setTimeout(
        () =>
          resolve({
            result: "failure",
            message:
              options.timeoutMessage || `Timeout trying to check port ${port}`,
          }),
        options.timeout ?? 1_000,
      )
    }),
  ])
}
