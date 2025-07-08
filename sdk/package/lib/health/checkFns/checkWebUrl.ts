import { Effects } from "../../../../base/lib/types"
import { asError } from "../../util"
import { HealthCheckResult } from "./HealthCheckResult"
import { timeoutPromise } from "./index"
import "isomorphic-fetch"

/**
 * This is a helper function to check if a web url is reachable.
 * @param url
 * @param createSuccess
 * @returns
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
