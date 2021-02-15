import { Observable, from, interval, race, OperatorFunction, Observer, combineLatest } from 'rxjs'
import { take, map, concatMap } from 'rxjs/operators'

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

// throws unless source observable emits withing timeout
export function throwIn<T> (timeout: number): OperatorFunction<T, T> {
  return o => race(
    o,
    emitAfter$(timeout).pipe(map(() => { throw new Error('timeout') } )))
}

// o.pipe(squash) : Observable<void> regardless of o emission type.
export const squash = map(() => { })

/*
  The main purpose of fromSync$ is to normalize error handling during a sequence
  of actions beginning with a standard synchronous action and followed by a pipe.
  For example, imagine we have `f(s: S): T` which might throw, and we wish to define the following:
  ```
  function observableF(t: T): Observable<any> {
    const s = f(t)
    return someFunctionReturningAnObservable(s)
  }
  ```

  For the caller, `observableF(t).pipe(...).subscribe({ error: e => console.error('observable errored!') })`
  might throw an error from `f` which does not result in 'observable errored!' being logged.
  We could fix this with...
  ```
  function observableF(t: T): Observable<any> {
    try {
      const s = f(t)
      return someFunctionReturningAnObservable(s)
    } catch(e) {
      return throwError(e)
    }
  }
  ```

  or we could use fromSync as below
  ```
  function observableF(t: T): Observable<any> {
    return fromSync$(f, t).concatMap(someFunctionReturningAnObservable)
  }
  ```
*/
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

/*
  this function concats the current values (e.g in behavior subjects) or next values (in traditional observables) to a collection of values in a pipe.
  e.g. if t: Observable<T>, and o1: Observable<O1> o2: Observable<O2> then t.pipe(concatObservableValues([o1, o2])): Observable<[T1, O1, O2]> and emits iff t emits.
  Note that the standard combineLatest([t, o1, o2]) is also of type Observable<[T, O2, O2]>, but this observable triggers when any of t, o1, o2 emits.
*/
export function concatObservableValues<T, O> (observables: [Observable<O>]): OperatorFunction<T, [T, O]>
export function concatObservableValues<T, O> (observables: [Observable<O>]): OperatorFunction<[T], [T, O]>
export function concatObservableValues<T1, T2, O> (observables: [Observable<O>]): OperatorFunction<[T1, T2], [T1, T2, O]>
export function concatObservableValues<T, O1, O2> (observables: [Observable<O1>, Observable<O2>]): OperatorFunction<[T], [T, O1, O2]>
export function concatObservableValues<T1, T2, O1, O2> (observables: [Observable<O1>, Observable<O2>]): OperatorFunction<[T1, T2], [T1, T2, O1, O2]>
export function concatObservableValues (observables: Observable<any>[]): OperatorFunction<any[], any[]> {
  return o => o.pipe(concatMap(args => combineLatest(observables).pipe(
      map(obs => {
        if (!(args instanceof Array)) return [args, ...obs]
        return [...args, ...obs]
      }),
      take(1),
    ),
  ))
}