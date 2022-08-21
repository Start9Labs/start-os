import { Inject, Injectable } from '@angular/core'
import { Bootstrapper, PatchDB, Store } from 'patch-db-client'
import { Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, tap } from 'rxjs/operators'
import { DataModel } from './data-model'
import { BOOTSTRAPPER } from './patch-db.factory'

@Injectable({
  providedIn: 'root',
})
export class PatchDbService {
  private sub?: Subscription

  constructor(
    @Inject(BOOTSTRAPPER)
    private readonly bootstrapper: Bootstrapper<DataModel>,
    private readonly patchDb: PatchDB<DataModel>,
  ) {}

  start(): void {
    // Early return if already started
    if (this.sub) {
      return
    }

    console.log('patchDB: STARTING')
    this.sub = this.patchDb.cache$
      .pipe(
        debounceTime(420),
        tap(cache => {
          this.bootstrapper.update(cache)
        }),
      )
      .subscribe()
  }

  stop(): void {
    // Early return if already stopped
    if (!this.sub) {
      return
    }

    console.log('patchDB: STOPPING')
    this.patchDb.store.reset()
    this.sub.unsubscribe()
    this.sub = undefined
  }

  // prettier-ignore
  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    const argsString = '/' + args.join('/')

    console.log('patchDB: WATCHING ', argsString)

    return this.patchDb.store.watch$(...(args as [])).pipe(
      tap(data => console.log('patchDB: NEW VALUE', argsString, data)),
      catchError(e => {
        console.error('patchDB: WATCH ERROR', e)
        return of(e.message)
      }),
      finalize(() => console.log('patchDB: UNSUBSCRIBING', argsString)),
    )
  }
}
