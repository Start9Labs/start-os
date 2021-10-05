import { BehaviorSubject, Observable, Subject } from 'rxjs'
import { concatMap, finalize } from 'rxjs/operators'
import { fromSync$, emitAfter$ } from 'src/app/util/rxjs.util'

export interface Loadable {
  load: (prevResult?: any) => void
  result?: any // fill this variable on slide 1 to get passed into the load on slide 2. If this variable is falsey, it will skip the next slide.
  loading$: BehaviorSubject<boolean> // will be true during load function
  cancel$: Subject<void> // will cancel load function
}

export function markAsLoadingDuring$<T> (trigger$: Subject<boolean>, o: Observable<T>): Observable<T> {
  let shouldBeOn = true
  const displayIfItsBeenAtLeast = 5 // ms
  return fromSync$(() => {
    emitAfter$(displayIfItsBeenAtLeast).subscribe(() => { if (shouldBeOn) trigger$.next(true) })
  }).pipe(
    concatMap(() => o),
    finalize(() => {
      trigger$.next(false)
      shouldBeOn = false
    }),
 )
}
