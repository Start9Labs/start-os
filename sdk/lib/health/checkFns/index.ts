import { runHealthScript } from "./runHealthScript"
export { checkPortListening } from "./checkPortListening"
export { CheckResult } from "./CheckResult"
export { checkWebUrl } from "./checkWebUrl"

export function timeoutPromise(ms: number, { message = "Timed out" } = {}) {
  return new Promise<never>((resolve, reject) =>
    setTimeout(() => reject(new Error(message)), ms),
  )
}
export { runHealthScript }
