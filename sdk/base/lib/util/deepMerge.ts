export function partialDiff<T>(
  prev: T,
  next: T,
): { diff: Partial<T> } | undefined {
  if (prev === next) {
    return
  } else if (Array.isArray(prev) && Array.isArray(next)) {
    const res = { diff: [] as any[] }
    for (let newItem of next) {
      let anyEq = false
      for (let oldItem of prev) {
        if (!partialDiff(oldItem, newItem)) {
          anyEq = true
          break
        }
      }
      if (!anyEq) {
        res.diff.push(newItem)
      }
    }
    if (res.diff.length) {
      return res as any
    } else {
      return
    }
  } else if (typeof prev === "object" && typeof next === "object") {
    if (prev === null || next === null) return { diff: next }
    const res = { diff: {} as Record<keyof T, any> }
    const keys = Object.keys(next) as (keyof T)[]
    for (let key in prev) {
      if (!keys.includes(key)) keys.push(key)
    }
    for (let key of keys) {
      const diff = partialDiff(prev[key], next[key])
      if (diff) {
        res.diff[key] = diff.diff
      }
    }
    if (Object.keys(res.diff).length) {
      return res
    } else {
      return
    }
  } else {
    return { diff: next }
  }
}

export function deepMerge(...args: unknown[]): unknown {
  const lastItem = (args as any)[args.length - 1]
  if (typeof lastItem !== "object" || !lastItem) return lastItem
  if (Array.isArray(lastItem))
    return deepMergeList(
      ...(args.filter((x) => Array.isArray(x)) as unknown[][]),
    )
  return deepMergeObject(
    ...(args.filter(
      (x) => typeof x === "object" && x && !Array.isArray(x),
    ) as object[]),
  )
}

function deepMergeList(...args: unknown[][]): unknown[] {
  const res: unknown[] = []
  for (let arg of args) {
    for (let item of arg) {
      if (!res.some((x) => !partialDiff(x, item))) {
        res.push(item)
      }
    }
  }
  return res
}

function deepMergeObject(...args: object[]): object {
  const lastItem = (args as any)[args.length - 1]
  if (args.length === 0) return lastItem as any
  if (args.length === 1) args.unshift({})
  const allKeys = new Set(args.flatMap((x) => Object.keys(x)))
  for (const key of allKeys) {
    const filteredValues = args.flatMap((x) =>
      key in x ? [(x as any)[key]] : [],
    )
    ;(args as any)[0][key] = deepMerge(...filteredValues)
  }
  return args[0] as any
}
