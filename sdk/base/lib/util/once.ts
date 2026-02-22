/**
 * Wraps a function so it is only executed once. Subsequent calls return the cached result.
 *
 * @param fn - The function to execute at most once
 * @returns A wrapper that lazily evaluates `fn` on first call and caches the result
 *
 * @example
 * ```ts
 * const getConfig = once(() => loadExpensiveConfig())
 * getConfig() // loads config
 * getConfig() // returns cached result
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
