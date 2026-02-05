/**
 * Creates a memoized version of a function that only executes once.
 * Subsequent calls return the cached result from the first invocation.
 *
 * Useful for lazy initialization where you want to defer computation
 * until first use, but then cache the result.
 *
 * @typeParam B - The return type of the function
 * @param fn - The function to execute once and cache
 * @returns A function that returns the cached result
 *
 * @example
 * ```typescript
 * const getExpensiveValue = once(() => {
 *   console.log('Computing...')
 *   return computeExpensiveValue()
 * })
 *
 * getExpensiveValue() // Logs "Computing...", returns value
 * getExpensiveValue() // Returns cached value, no log
 * getExpensiveValue() // Returns cached value, no log
 * ```
 */
export function once<B>(fn: () => B): () => B {
  let result: [B] | [] = []
  return () => {
    if (!result.length) {
      result = [fn()]
    }
    return result[0]
  }
}
