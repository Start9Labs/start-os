import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, map, tap } from 'rxjs/operators'
import { ApiService } from '../api/embassy-api.service'
import { DataModel } from './data-model'

export const PATCH_HTTP = new InjectionToken<Source<DataModel>>('app.config')
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('app.config')
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('app.config')

export enum PatchConnection {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class PatchDbService {
  patchConnection$ = new BehaviorSubject(PatchConnection.Initializing)
  private patchDb: PatchDB<DataModel>
  private patchSub: Subscription
  data: DataModel

  getData () { return this.patchDb.store.cache.data }

  constructor (
    @Inject(PATCH_SOURCE) private readonly source: Source<DataModel>,
    @Inject(PATCH_HTTP) private readonly http: ApiService,
    @Inject(BOOTSTRAPPER) private readonly bootstrapper: Bootstrapper<DataModel>,
  ) { }

  async init (): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB([this.source, this.http], this.http, cache)
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
          this.patchConnection$.next(PatchConnection.Connected)
          this.bootstrapper.update(cache)
        },
        error: e => {
          console.error('patch-db-sync sub ERROR', e)
          this.patchConnection$.next(PatchConnection.Disconnected)
          // this.start()
        },
        complete: () => {
          console.warn('patch-db-sync sub COMPLETE')
        },
      })
    } catch (e) {
      console.error('Failed to initialize PatchDB', e)
    }
  }

  stop (): void {
    this.patchConnection$.next(PatchConnection.Initializing)
    if (this.patchSub) {
      this.patchSub.unsubscribe()
      this.patchSub = undefined
    }
  }

  connected$ (): Observable<boolean> {
    return this.patchConnection$
    .pipe(
      map(status => status === PatchConnection.Connected),
    )
  }

  watchPatchConnection$ (): Observable<PatchConnection> {
    return this.patchConnection$.asObservable()
  }

  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    console.log('WATCHING', ...args)
    return this.patchDb.store.watch$(...(args as []))
    .pipe(
      tap(data => console.log('NEW VALUE', data, ...args)),
      catchError(e => {
        console.error('Error watching Patch DB', e)
        return of(e.message)
      }),
      finalize(() => console.log('UNSUBSCRIBING', ...args)),
    )
  }
}
