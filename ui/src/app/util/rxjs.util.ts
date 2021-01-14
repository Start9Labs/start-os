import { Observable, from, interval, race, OperatorFunction, Observer, BehaviorSubject } from 'rxjs'
import { take, map, switchMap, delay, tap } from 'rxjs/operators'

export function fromAsync$<S, T> (async: (s: S) => Promise<T>, s: S): Observable<T>
export function fromAsync$<T> (async: () => Promise<T>): Observable<T>
export function fromAsync$<S, T> (async: (s: S) => Promise<T>, s?: S): Observable<T> {
  return from(async(s as S))
}

export function fromAsyncP<T> (async: () => Promise<T>): Promise<T>
export function fromAsyncP<S, T> (async: (s: S) => Promise<T>, s?: S): Promise<T> {
  return async(s as S)
}

// emits + completes after ms
export function emitAfter$ (ms: number): Observable<number> {
  return interval(ms).pipe(take(1))
}

export function throwIn<T> (timeout: number): OperatorFunction<T, T> {
  return o => race(
    o,
    emitAfter$(timeout).pipe(map(() => { throw new Error('timeout') } )))
}

export const squash = map(() => { })

export function fromSync$<S, T> (sync: (s: S) => T, s: S): Observable<T>
export function fromSync$<T> (sync: () => T): Observable<T>
export function fromSync$<S, T> (sync: (s: S) => T, s?: S): Observable<T> {
  return new Observable( (subscriber: Observer<T>) => {
    try {
      subscriber.next(sync(s as S))
      subscriber.complete()
    } catch (e) {
      subscriber.error(e)
    }
  })
}

export function onCooldown<T> (cooldown: number, o: () => Observable<T>): Observable<T> {

  const $trigger$ = new BehaviorSubject(true)
  $trigger$.subscribe(t => console.log('triggering', t))
  return $trigger$.pipe(
    switchMap(_ =>
      o().pipe(
        delay(cooldown),
        tap(() => $trigger$.next(true)),
      ),
    ),
  )
}
