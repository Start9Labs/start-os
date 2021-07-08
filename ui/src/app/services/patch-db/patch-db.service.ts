import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, map, tap } from 'rxjs/operators'
import { ApiService } from '../api/api.service'
import { DataModel } from './data-model'

export const PATCH_HTTP = new InjectionToken<Source<DataModel>>('app.config')
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('app.config')
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('app.config')

export enum ConnectionStatus {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class PatchDbModel {
  connectionStatus$ = new BehaviorSubject(ConnectionStatus.Initializing)
  sequence$: Observable<number>
  data: DataModel
  private patchDb: PatchDB<DataModel>
  private patchSub: Subscription

  constructor (
    @Inject(PATCH_SOURCE) private readonly source: Source<DataModel>,
    @Inject(PATCH_HTTP) private readonly http: ApiService,
    @Inject(BOOTSTRAPPER) private readonly bootstrapper: Bootstrapper<DataModel>,
  ) { }

  async init (): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB([this.source, this.http], this.http, cache)

    this.sequence$ = this.patchDb.store.sequence$.asObservable()
    this.data = this.patchDb.store.cache.data
  }

  start (): void {
    // make sure everything is stopped before initializing
    this.stop()
    try {
      this.patchSub = this.patchDb.sync$()
      .pipe(debounceTime(500))
      .subscribe({
        next: cache => {
          console.log('saving cacheee: ', cache)
          this.connectionStatus$.next(ConnectionStatus.Connected)
          this.bootstrapper.update(cache)
        },
        error: e => {
          console.error('patch-db-sync sub ERROR', e)
          this.connectionStatus$.next(ConnectionStatus.Disconnected)
          // this.start()
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
    return this.connectionStatus$
    .pipe(
      map(status => status === ConnectionStatus.Connected),
    )
  }

  watchConnection$ (): Observable<ConnectionStatus> {
    return this.connectionStatus$.asObservable()
  }

  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    console.log('WATCHING', ...args)
    return this.patchDb.store.watch$(...(args as [])).pipe(
      tap(cache => console.log('CHANGE IN STORE', cache)),
      catchError(e => {
        console.error(e)
        return of(e.message)
      }),
      finalize(() => console.log('UNSUBSCRIBING')),
    )
  }
}
