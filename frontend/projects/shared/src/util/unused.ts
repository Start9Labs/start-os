import { map, OperatorFunction } from 'rxjs'

/**
 * These utils are not used anywhere
 * They are candidates for removal
 */

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
): number {
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

export function toObject<T>(t: T[], map: (t0: T) => string): Record<string, T> {
  return t.reduce((acc, next) => {
    acc[map(next)] = next
    return acc
  }, {} as Record<string, T>)
}

export function partitionArray<T>(
  ts: T[],
  condition: (t: T) => boolean,
): [T[], T[]] {
  const yes: T[] = []
  const no: T[] = []
  ts.forEach(t => {
    if (condition(t)) {
      yes.push(t)
    } else {
      no.push(t)
    }
  })
  return [yes, no]
}

export function update<T>(
  t: Record<string, T>,
  u: Record<string, T>,
): Record<string, T> {
  return { ...t, ...u }
}
