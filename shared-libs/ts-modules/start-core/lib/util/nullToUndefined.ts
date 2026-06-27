/** Recursively convert null values to undefined */
export type NullToUndefined<T> = T extends null
  ? undefined
  : T extends (infer U)[]
    ? NullToUndefined<U>[]
    : T extends object
      ? { [K in keyof T]: NullToUndefined<T[K]> }
      : T

export function nullToUndefined<T>(obj: T): NullToUndefined<T> {
  if (obj === null) return undefined as NullToUndefined<T>
  if (Array.isArray(obj)) return obj.map(nullToUndefined) as NullToUndefined<T>
  if (typeof obj === 'object') {
    const result: Record<string, unknown> = {}
    for (const [k, v] of Object.entries(obj as Record<string, unknown>)) {
      result[k] = nullToUndefined(v)
    }
    return result as NullToUndefined<T>
  }
  return obj as NullToUndefined<T>
}
