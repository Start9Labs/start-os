export function invert<
  T extends string | number | symbol,
  D extends string | number | symbol,
>(obj: Record<T, D>): Record<D, T> {
  const result = {} as Record<D, T>

  for (const key in obj) {
    result[obj[key]] = key
  }

  return result
}
