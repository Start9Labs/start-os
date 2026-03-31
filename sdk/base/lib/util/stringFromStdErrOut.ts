/**
 * Extracts a string result from a stdout/stderr pair.
 * Returns `stdout` on success; rejects with `stderr` if it is non-empty.
 *
 * @param x - An object containing `stdout` and `stderr` strings
 * @returns A promise resolving to `stdout`, or rejecting with `stderr`
 */
export async function stringFromStdErrOut(x: {
  stdout: string
  stderr: string
}) {
  return x?.stderr ? Promise.reject(x.stderr) : x.stdout
}
