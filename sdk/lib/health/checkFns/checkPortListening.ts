import { Effects } from "../../types"
import { stringFromStdErrOut } from "../../util/stringFromStdErrOut"
import { CheckResult } from "./CheckResult"

import { promisify } from "node:util"
import * as CP from "node:child_process"

const cpExec = promisify(CP.exec)
const cpExecFile = promisify(CP.execFile)
export function containsAddress(x: string, port: number) {
  const readPorts = x
    .split("\n")
    .filter(Boolean)
    .splice(1)
    .map((x) => x.split(" ").filter(Boolean)[1]?.split(":")?.[1])
    .filter(Boolean)
    .map((x) => Number.parseInt(x, 16))
    .filter(Number.isFinite)
  return readPorts.indexOf(port) >= 0
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
): Promise<CheckResult> {
  return Promise.race<CheckResult>([
    Promise.resolve().then(async () => {
      const hasAddress =
        containsAddress(
          await cpExec(`cat /proc/net/tcp`, {}).then(stringFromStdErrOut),
          port,
        ) ||
        containsAddress(
          await cpExec("cat /proc/net/udp", {}).then(stringFromStdErrOut),
          port,
        )
      if (hasAddress) {
        return { status: "success", message: options.successMessage }
      }
      return {
        status: "failure",
        message: options.errorMessage,
      }
    }),
    new Promise((resolve) => {
      setTimeout(
        () =>
          resolve({
            status: "failure",
            message:
              options.timeoutMessage || `Timeout trying to check port ${port}`,
          }),
        options.timeout ?? 1_000,
      )
    }),
  ])
}
