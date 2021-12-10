import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, concatMap, debounceTime, finalize, map, tap } from 'rxjs/operators'
import { pauseFor } from 'src/app/util/misc.util'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { DataModel } from './data-model'

export const HTTP_SOURCE = new InjectionToken<Source<DataModel>>('')
export const WS_SOURCE = new InjectionToken<Source<DataModel>>('')
export const POLL_SOURCE = new InjectionToken<Source<DataModel>>('')
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
  polling: boolean

  getData () { return this.patchDb.store.cache.data }

  constructor (
    @Inject(WS_SOURCE) private readonly wsSource: Source<DataModel>,
    @Inject(POLL_SOURCE) private readonly pollSource: Source<DataModel>,
    @Inject(HTTP_SOURCE) private readonly http: ApiService,
    @Inject(BOOTSTRAPPER) private readonly bootstrapper: Bootstrapper<DataModel>,
    @Inject(AUTH) private readonly auth: AuthService,
  ) { }

  async init (): Promise<void> {
    await this.initSource(this.wsSource)
  }

  async initSource (source: Source<DataModel>): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB([source, this.http], this.http, cache)
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
      debounceTime(420),
      tap(cache => {
        this.bootstrapper.update(cache)
      }),
      concatMap(() => this.patchConnection$),
    )
    .subscribe({
      next: async connection => {
        if (!this.polling) {
          console.log('CON: WEBSOCKET SUCCESS')
          localStorage.setItem('wsSuccess', 'true')
        }

        if (connection === PatchConnection.Disconnected && !!localStorage.getItem('wsSuccess')) {
          console.log('CON: SWITCHING BACK TO WEBSOCKETS')
          this.polling = false
          this.patchConnection$.next(PatchConnection.Initializing)
          await this.initSource(this.wsSource)
          this.start()
        } else if (connection !== PatchConnection.Connected) {
          console.log(this.polling ? 'CON: POLL CONNECTED' : 'CON: WEBSOCKET CONNECTED')
          this.patchConnection$.next(PatchConnection.Connected)
        }
      },
      error: async e => {
        console.error('patch-db SYNC ERROR', e)
        if (e.code === 34) {
          this.auth.setUnverified()
        } else {
          if (this.polling) {
            console.log('CON: POLLING FAILED')
            this.patchConnection$.next(PatchConnection.Disconnected)
            await pauseFor(2000)
          } else {
            console.log('CON: WEBSOCKET FAILED')
            this.patchConnection$.next(PatchConnection.Initializing)
            this.polling = true
            await this.initSource(this.pollSource)
          }
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
