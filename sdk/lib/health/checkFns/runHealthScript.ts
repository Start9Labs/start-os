import { Effects } from "../../types"
import { Overlay } from "../../util/Overlay"
import { stringFromStdErrOut } from "../../util/stringFromStdErrOut"
import { CheckResult } from "./CheckResult"
import { timeoutPromise } from "./index"

/**
 * Running a health script, is used when we want to have a simple
 * script in bash or something like that. It should return something that is useful
 * in {result: string} else it is considered an error
 * @param param0
 * @returns
 */
export const runHealthScript = async (
  effects: Effects,
  runCommand: string[],
  overlay: Overlay,
  {
    timeout = 30000,
    errorMessage = `Error while running command: ${runCommand}`,
    message = (res: string) =>
      `Have ran script ${runCommand} and the result: ${res}`,
  } = {},
): Promise<CheckResult> => {
  const res = await Promise.race([
    overlay.exec(runCommand),
    timeoutPromise(timeout),
  ]).catch((e) => {
    console.warn(errorMessage)
    console.warn(JSON.stringify(e))
    console.warn(e.toString())
    throw { status: "failure", message: errorMessage } as CheckResult
  })
  return {
    status: "passing",
    message: message(res.stdout.toString()),
  } as CheckResult
}
