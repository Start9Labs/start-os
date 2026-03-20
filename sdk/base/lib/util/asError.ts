/**
 * Converts an unknown thrown value into an Error instance.
 * If `e` is already an Error, wraps it; if a string, uses it as the message;
 * otherwise JSON-serializes it as the error message.
 *
 * @param e - The unknown value to convert
 * @returns An Error instance
 */
export const asError = (e: unknown) => {
  if (e instanceof Error) {
    return e
  }
  if (typeof e === 'string') {
    return new Error(`${e}`)
  }
  return new Error(`${JSON.stringify(e)}`)
}
