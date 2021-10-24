import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, map, tap } from 'rxjs/operators'
import { pauseFor } from 'src/app/util/misc.util'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { DataModel } from './data-model'

export const PATCH_HTTP = new InjectionToken<Source<DataModel>>('')
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('')
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('')
export const AUTH = new InjectionToken<AuthService>('')

export enum PatchConnection {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class PatchDbService {
  private patchConnection$ = new BehaviorSubject(PatchConnection.Initializing)
  private patchDb: PatchDB<DataModel>
  private patchSub: Subscription
  data: DataModel

  getData () { return this.patchDb.store.cache.data }

  constructor (
    @Inject(PATCH_SOURCE) private readonly source: Source<DataModel>,
    @Inject(PATCH_HTTP) private readonly http: ApiService,
    @Inject(BOOTSTRAPPER) private readonly bootstrapper: Bootstrapper<DataModel>,
    @Inject(AUTH) private readonly auth: AuthService,
  ) { }

  async init (): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB([this.source, this.http], this.http, cache)
    this.data = this.patchDb.store.cache.data
  }

  start (): void {
    console.log(this.patchSub ? 'restarting patch-db' : 'starting patch-db')

    // make sure everything is stopped before initializing
    if (this.patchSub) {
      this.patchSub.unsubscribe()
      this.patchSub = undefined
    }

    this.patchSub = this.patchDb.sync$()
    .pipe(
      debounceTime(400),
      tap(cache => {
        this.patchConnection$.next(PatchConnection.Connected)
        this.bootstrapper.update(cache)
      }),
    )
    .subscribe({
      error: async e => {
        console.error('patch-db SYNC ERROR', e)
        this.patchConnection$.next(PatchConnection.Disconnected)
        if (e.code === 34) {
          this.auth.setUnverified()
        } else {
          await pauseFor(4000)
          this.start()
        }
      },
      complete: () => {
        console.warn('patch-db SYNC COMPLETE')
      },
    })
  }

  stop (): void {
    console.log('stopping patch-db')
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
        console.error('Error watching patch-db', e)
        return of(e.message)
      }),
      finalize(() => console.log('UNSUBSCRIBING', ...args)),
    )
  }
}
