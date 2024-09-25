import { object } from "ts-matches"

export function deepEqual(...args: unknown[]) {
  if (!object.test(args[args.length - 1])) return args[args.length - 1]
  const objects = args.filter(object.test)
  if (objects.length === 0) {
    for (const x of args) if (x !== args[0]) return false
    return true
  }
  if (objects.length !== args.length) return false
  const allKeys = new Set(objects.flatMap((x) => Object.keys(x)))
  for (const key of allKeys) {
    for (const x of objects) {
      if (!(key in x)) return false
      if (!deepEqual((objects[0] as any)[key], (x as any)[key])) return false
    }
  }
  return true
}
