import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, map, tap, withLatestFrom } from 'rxjs/operators'
import { pauseFor } from 'src/app/util/misc.util'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { DataModel } from './data-model'


export enum PatchConnection {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class PatchDbService {
  private readonly WS_SUCCESS = 'wsSuccess'
  private patchConnection$ = new BehaviorSubject(PatchConnection.Initializing)
  private wsSuccess$ = new BehaviorSubject(false)
  private polling$ = new BehaviorSubject(false)
  private patchDb: PatchDB<DataModel>
  private patchSub: Subscription
  data: DataModel

  getData () { return this.patchDb.store.cache.data }

  constructor (
    private readonly wsSource: Source<DataModel>,
    private readonly pollSource: Source<DataModel>,
    private readonly http: ApiService,
    private readonly bootstrapper: Bootstrapper<DataModel>,
    private readonly auth: AuthService,
    private readonly storage: Storage,
  ) { }

  async init (): Promise<void> {
    await this.initSource(this.wsSource)
    const wsSuccess = !!(await this.storage.get(this.WS_SUCCESS))
    this.wsSuccess$.next(wsSuccess)
  }

  async initSource (source: Source<DataModel>): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.patchDb = new PatchDB([source, this.http], this.http, cache)
    this.data = this.patchDb.store.cache.data
  }

  start (): void {
    console.log(this.patchSub ? 'patchDB: RESTARTING' : 'patchDB: STARTING')

    // make sure everything is stopped before initializing
    if (this.patchSub) {
      this.patchSub.unsubscribe()
      this.patchSub = undefined
    }

    this.patchSub = this.patchDb.sync$()
    .pipe(
      debounceTime(420),
      withLatestFrom(this.patchConnection$, this.wsSuccess$, this.polling$),
      tap(async ([cache, connection, wsSuccess, polling]) => {
        this.bootstrapper.update(cache)

        if (connection === PatchConnection.Initializing) {
          console.log(polling ? 'patchDB: POLL CONNECTED' : 'patchDB: WEBSOCKET CONNECTED')
          this.patchConnection$.next(PatchConnection.Connected)
          if (!wsSuccess && !polling) {
            console.log('patchDB: WEBSOCKET SUCCESS')
            this.storage.set(this.WS_SUCCESS, 'true')
            this.wsSuccess$.next(true)
          }
        } else if (connection === PatchConnection.Disconnected && wsSuccess) {
          console.log('patchDB: SWITCHING BACK TO WEBSOCKETS')
          this.polling$.next(false)
          this.patchConnection$.next(PatchConnection.Initializing)
          await this.initSource(this.wsSource)
          this.start()
        }
      }),
      catchError(e => {
        if (e.code === 34) {
          console.log('patchDB: Unauthorized. Logging out.')
          this.auth.setUnverified()
        } else {
          return of([])
          .pipe(
            withLatestFrom(this.polling$),
            tap(async ([e, polling]) => {
              if (polling) {
                console.log('patchDB: POLLING FAILED', e)
                this.patchConnection$.next(PatchConnection.Disconnected)
                await pauseFor(2000)
              } else {
                console.log('patchDB: WEBSOCKET FAILED', e)
                this.patchConnection$.next(PatchConnection.Initializing)
                this.polling$.next(true)
                await this.initSource(this.pollSource)
              }
              this.start()
            }),
          )
        }
      }),
    )
    .subscribe({
      complete: () => {
        console.warn('patchDB: SYNC COMPLETE')
      },
    })
  }

  stop (): void {
    console.log('patchDB: STOPPING')
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
    const argsString = args.join('/') || '/'
    console.log('patchDB: WATCHING ', argsString)
    return this.patchDb.store.watch$(...(args as []))
    .pipe(
      tap(data => console.log('patchDB: NEW VALUE', argsString, data)),
      catchError(e => {
        console.error('patchDB: WATCH ERROR', e)
        return of(e.message)
      }),
      finalize(() => console.log('patchDB: UNSUBSCRIBING', argsString)),
    )
  }
}
