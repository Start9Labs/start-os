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
    .then((x) => ({
      status: "passing" as const,
      message: successMessage,
    }))
    .catch((e) => {
      console.warn(`Error while fetching URL: ${url}`)
      console.error(JSON.stringify(e))
      console.error(e.toString())
      return { status: "failing" as const, message: errorMessage }
    })
}
