import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Bootstrapper, ConnectionStatus, PatchDB, Source, Store } from 'patch-db-client'
import { Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, map } from 'rxjs/operators'
import { DataModel } from './data-model'

export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('app.config')
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('app.config')

@Injectable({
  providedIn: 'root',
})
export class PatchDbModel {
  patchDb: PatchDB<DataModel>
  private patchSub: Subscription

  constructor (
    @Inject(BOOTSTRAPPER) private readonly bootstrapper: Bootstrapper<DataModel>,
    @Inject(PATCH_SOURCE) private readonly source: Source<DataModel>,
  ) { }

  async init (): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB(this.source, cache)
  }

  start (): void {
    // make sure everything is stopped before initializing
    this.stop()
    try {
      this.patchSub = this.patchDb.sync$()
      .pipe(debounceTime(500))
      .subscribe({
        next: cache => {
          console.log('saving cache: ', cache.sequence)
          this.bootstrapper.update(cache)
        },
        error: e => {
          console.error('patch-db-sync sub ERROR', e)
          this.start()
        },
        complete: () => {
          console.error('patch-db-sync sub COMPLETE')
        },
      })
    } catch (e) {
      console.log('Failed to initialize PatchDB', e)
    }
  }

  stop (): void {
    if (this.patchSub) {
      this.patchSub.unsubscribe()
      this.patchSub = undefined
    }
  }

  connected$ (): Observable<boolean> {
    return this.patchDb.connectionStatus$
    .pipe(
      map(status => status === ConnectionStatus.Connected),
    )
  }

  connectionStatus$ (): Observable<ConnectionStatus> {
    return this.patchDb.connectionStatus$.asObservable()
  }

  watch$: Store<DataModel> ['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    // console.log('WATCHING')
    return this.patchDb.store.watch$(...(args as [])).pipe(
      catchError(e => {
        console.error(e)
        return of(e.message)
      }),
      // finalize(() => console.log('unSUBSCRIBing')),
    )
  }
}
