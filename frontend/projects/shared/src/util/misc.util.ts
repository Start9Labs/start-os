import { OperatorFunction } from 'rxjs'
import { map } from 'rxjs/operators'

export function trace<T>(t: T): T {
  console.log(`TRACE`, t)
  return t
}

// curried description. This allows e.g somePromise.thentraceDesc('my result'))
export function traceDesc<T>(description: string): (t: T) => T {
  return t => {
    console.log(`TRACE`, description, t)
    return t
  }
}

// for use in observables. This allows e.g. someObservable.pipe(traceM('my result'))
// the practical equivalent of `tap(t => console.log(t, description))`
export function traceWheel<T>(description?: string): OperatorFunction<T, T> {
  return description ? map(traceDesc(description)) : map(trace)
}

export function traceThrowDesc<T>(description: string, t: T | undefined): T {
  if (!t) throw new Error(description)
  return t
}

export function inMs(
  count: number,
  unit: 'days' | 'hours' | 'minutes' | 'seconds',
) {
  switch (unit) {
    case 'seconds':
      return count * 1000
    case 'minutes':
      return inMs(count * 60, 'seconds')
    case 'hours':
      return inMs(count * 60, 'minutes')
    case 'days':
      return inMs(count * 24, 'hours')
  }
}

// arr1 - arr2
export function diff<T>(arr1: T[], arr2: T[]): T[] {
  return arr1.filter(x => !arr2.includes(x))
}

// arr1 & arr2
export function both<T>(arr1: T[], arr2: T[]): T[] {
  return arr1.filter(x => arr2.includes(x))
}

export function isObject(val: any): boolean {
  return val && typeof val === 'object' && !Array.isArray(val)
}

export function isEmptyObject(obj: object): boolean {
  if (obj === undefined) return true
  return !Object.keys(obj).length
}

export function pauseFor(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}

export function toObject<T>(t: T[], map: (t0: T) => string): Record<string, T> {
  return t.reduce((acc, next) => {
    acc[map(next)] = next
    return acc
  }, {} as Record<string, T>)
}

export function update<T>(
  t: Record<string, T>,
  u: Record<string, T>,
): Record<string, T> {
  return { ...t, ...u }
}

export function deepCloneUnknown<T>(value: T): T {
  if (typeof value !== 'object' || value === null) {
    return value
  }
  if (Array.isArray(value)) {
    return deepCloneArray(value)
  }
  return deepCloneObject(value)
}

export function deepCloneObject<T>(source: T) {
  const result = {}
  Object.keys(source).forEach(key => {
    const value = source[key]
    result[key] = deepCloneUnknown(value)
  }, {})
  return result as T
}

export function deepCloneArray(collection: any) {
  return collection.map(value => {
    return deepCloneUnknown(value)
  })
}

export function partitionArray<T>(
  ts: T[],
  condition: (t: T) => boolean,
): [T[], T[]] {
  const yes = [] as T[]
  const no = [] as T[]
  ts.forEach(t => {
    if (condition(t)) {
      yes.push(t)
    } else {
      no.push(t)
    }
  })
  return [yes, no]
}

export function uniqueBy<T>(
  ts: T[],
  uniqueBy: (t: T) => string,
  prioritize: (t1: T, t2: T) => T,
) {
  return Object.values(
    ts.reduce((acc, next) => {
      const previousValue = acc[uniqueBy(next)]
      if (previousValue) {
        acc[uniqueBy(next)] = prioritize(acc[uniqueBy(next)], previousValue)
      } else {
        acc[uniqueBy(next)] = previousValue
      }
      return acc
    }, {}),
  )
}

export function capitalizeFirstLetter(string: string): string {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

export const exists = (t: any) => {
  return t !== undefined
}

export function debounce(delay: number = 300): MethodDecorator {
  return function (
    target: any,
    propertyKey: string,
    descriptor: PropertyDescriptor,
  ) {
    const timeoutKey = Symbol()

    const original = descriptor.value

    descriptor.value = function (...args) {
      clearTimeout(this[timeoutKey])
      this[timeoutKey] = setTimeout(() => original.apply(this, args), delay)
    }

    return descriptor
  }
}
