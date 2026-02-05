/**
 * Converts an unknown value to an Error instance.
 *
 * Handles various error formats commonly encountered in JavaScript:
 * - Error instances are returned as-is (re-wrapped)
 * - Strings become Error messages
 * - Other values are JSON-stringified into the Error message
 *
 * @param e - The unknown value to convert
 * @returns An Error instance representing the input
 *
 * @example
 * ```typescript
 * try {
 *   await someOperation()
 * } catch (e) {
 *   const error = asError(e)
 *   console.error(error.message)
 * }
 *
 * // Works with any thrown value
 * asError(new Error('oops'))        // Error: oops
 * asError('string error')           // Error: string error
 * asError({ code: 500 })            // Error: {"code":500}
 * ```
 */
export const asError = (e: unknown) => {
  if (e instanceof Error) {
    return new Error(e as any)
  }
  if (typeof e === "string") {
    return new Error(`${e}`)
  }
  return new Error(`${JSON.stringify(e)}`)
}
