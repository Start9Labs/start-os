export function getNewEntries<T extends Record<any, any>>(prev: T, curr: T): T {
  return Object.entries(curr).reduce(
    (result, [key, value]) =>
      prev[key]
        ? result
        : {
            ...result,
            [key]: value,
          },
    {} as T,
  )
}
