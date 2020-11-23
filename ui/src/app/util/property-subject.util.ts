import { BehaviorSubject, Observable, combineLatest, of } from 'rxjs'
import { map } from 'rxjs/operators'

export type PropertySubjectId<T> = {
  id: string
  subject: PropertySubject<T>
}

export type PropertySubject<T> = {
  [k in keyof T]: BehaviorSubject<T[k]>
}

// better type information than Object.entries without the return type cast
export function asLabelledList<T> (p : PropertySubject<T>): [string, BehaviorSubject<any>][] {
  return Object.entries(p)
}

export function peekProperties<T> (ps: PropertySubject<T>) : T {
  return asLabelledList(ps).reduce( (acc, [key, value]) => {
    acc[key] = value.getValue()
    return acc
  }, { } as T)
}

export function initPropertySubject<T> (t: T): PropertySubject<T> {
  return Object.entries(t).reduce( (acc, [k, v]) => {
    acc[k] = new BehaviorSubject(v)
    return acc
  }, { } as PropertySubject<T> )
}

export function withKey<V> (k: string, v: BehaviorSubject<V>): Observable<[string, V]> {
  return combineLatest([of(k), v])
}

export function toObservable<T> (t: PropertySubject<T>): Observable<T> {
  return combineLatest(
    asLabelledList(t as any).map(([k, p]) => withKey(k, p)),
  ).pipe(map( kvPairs => {
    return kvPairs.reduce( (acc, [k, v]) => {
      acc[k] = v
      return acc
    }, { }) as T
  }))
}

export function complete<T> (t: PropertySubject<T>): void {
  asLabelledList(t as any).forEach(p => p[1].complete() )
}
