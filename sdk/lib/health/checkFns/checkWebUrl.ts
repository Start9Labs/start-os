import { Effects } from "../../types"
import { CheckResult } from "./CheckResult"
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
): Promise<CheckResult> => {
  return Promise.race([fetch(url), timeoutPromise(timeout)])
    .then(
      (x) =>
        ({
          status: "success",
          message: successMessage,
        }) as const,
    )
    .catch((e) => {
      console.warn(`Error while fetching URL: ${url}`)
      console.error(JSON.stringify(e))
      console.error(e.toString())
      return { status: "failure" as const, message: errorMessage }
    })
}