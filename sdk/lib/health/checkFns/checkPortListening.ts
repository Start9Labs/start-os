import { Effects } from "../../types"
import { createUtils } from "../../util"
import { stringFromStdErrOut } from "../../util/stringFromStdErrOut"
import { CheckResult } from "./CheckResult"
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
  const utils = createUtils(effects)
  return Promise.race<CheckResult>([
    Promise.resolve().then(async () => {
      const hasAddress =
        containsAddress(
          await utils.childProcess
            .exec(`cat /proc/net/tcp`, {})
            .then(stringFromStdErrOut),
          port,
        ) ||
        containsAddress(
          await utils.childProcess
            .exec("cat /proc/net/udp", {})
            .then(stringFromStdErrOut),
          port,
        )
      if (hasAddress) {
        return { status: "passing", message: options.successMessage }
      }
      return {
        status: "failing",
        message: options.errorMessage,
      }
    }),
    new Promise((resolve) => {
      setTimeout(
        () =>
          resolve({
            status: "failing",
            message:
              options.timeoutMessage || `Timeout trying to check port ${port}`,
          }),
        options.timeout ?? 1_000,
      )
    }),
  ])
}
