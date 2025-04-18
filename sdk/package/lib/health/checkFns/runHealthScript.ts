import { HealthCheckResult } from "./HealthCheckResult"
import { timeoutPromise } from "./index"
import { SubContainer } from "../../util/SubContainer"
import { SDKManifest } from "../../types"

/**
 * Running a health script, is used when we want to have a simple
 * script in bash or something like that. It should return something that is useful
 * in {result: string} else it is considered an error
 * @param param0
 * @returns
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
    subcontainer.exec(runCommand),
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
