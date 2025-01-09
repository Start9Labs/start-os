import { Effects } from "../../../../base/lib/types"
import { stringFromStdErrOut } from "../../util"
import { HealthCheckResult } from "./HealthCheckResult"
import { promisify } from "node:util"
import * as CP from "node:child_process"

const cpExec = promisify(CP.exec)

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
 * This is used to check if a port is listening on the system.
 * Used during the health check fn or the check main fn.
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
