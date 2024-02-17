import { object } from "ts-matches"

export function deepMerge(...args: unknown[]): unknown {
  const lastItem = (args as any)[args.length - 1]
  if (!object.test(lastItem)) return lastItem
  const objects = args.filter(object.test).filter((x) => !Array.isArray(x))
  if (objects.length === 0) return lastItem as any
  if (objects.length === 1) objects.unshift({})
  const allKeys = new Set(objects.flatMap((x) => Object.keys(x)))
  for (const key of allKeys) {
    const filteredValues = objects.flatMap((x) =>
      key in x ? [(x as any)[key]] : [],
    )
    ;(objects as any)[0][key] = deepMerge(...filteredValues)
  }
  return objects[0] as any
}
