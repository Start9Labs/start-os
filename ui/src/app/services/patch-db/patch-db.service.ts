import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import { catchError, debounceTime, finalize, mergeMap, tap, withLatestFrom } from 'rxjs/operators'
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
  private subs: Subscription[] = []
  private sources$: BehaviorSubject<Source<DataModel>[]> = new BehaviorSubject([this.wsSource])

  data: DataModel
  errors = 0

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
    const cache = await this.bootstrapper.init()
    this.sources$.next([this.wsSource, this.http])

    this.patchDb = new PatchDB(this.sources$, this.http, cache)

    this.patchConnection$.next(PatchConnection.Initializing)
    this.data = this.patchDb.store.cache.data
  }

  async start (): Promise<void> {
    await this.init()

    console.log('PATCH DB', this.patchDb)

    this.subs.push(

      // Connection Error
      this.patchDb.connectionError$
      .pipe(
        debounceTime(420),
        withLatestFrom(this.polling$),
        mergeMap(async ([e, polling]) => {
          if (polling) {
            console.log('patchDB: POLLING FAILED', e)
            this.patchConnection$.next(PatchConnection.Disconnected)
            await pauseFor(2000)
            this.sources$.next([this.pollSource, this.http])
            return
          }

          console.log('patchDB: WEBSOCKET FAILED', e)
          this.polling$.next(true)
          this.sources$.next([this.pollSource, this.http])
        }),
      )
      .subscribe({
        complete: () => {
          console.warn('patchDB: SYNC COMPLETE')
        },
      }),


      // RPC ERROR
      this.patchDb.rpcError$
        .pipe(
          tap(({ error }) => {
            if (error.code === 34) {
              console.log('patchDB: Unauthorized. Logging out.')
              this.auth.setUnverified()
            }

          }),
        )
        .subscribe({
          complete: () => {
            console.warn('patchDB: SYNC COMPLETE')
          },
        }),

        // GOOD CONNECTION
        this.patchDb.cache$
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
                this.patchConnection$.next(PatchConnection.Initializing)
                this.polling$.next(false)
                this.sources$.next([this.wsSource, this.http])
              }
            }),
          )
          .subscribe({
            complete: () => {
              console.warn('patchDB: SYNC COMPLETE')
            },
          }),
    )
  }

  stop (): void {
    if (this.patchDb) {
      console.log('patchDB: STOPPING')
      this.patchConnection$.next(PatchConnection.Initializing)
      this.patchDb.store.reset()
    }
    this.subs.forEach(x => x.unsubscribe())
    this.subs = []
  }

  watchPatchConnection$ (): Observable<PatchConnection> {
    return this.patchConnection$.asObservable()
  }

  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    const argsString = '/' + args.join('/')
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


function switchMapAfter<A, B> (projection: (a: A) => Observable<B>) {
  return (observable: Observable<A>) =>
    new Observable<B>((subscriber) => {
      let lastLive: null | Subscription = null
      const subscription = observable.subscribe({
        next (value) {
          console.log('switchMapAfter JCWM new events')
          const lastLiveValue = lastLive
          lastLive = projection(value).subscribe(subscriber)
          if (!!lastLiveValue) {
            lastLiveValue.unsubscribe()
          }
        },
      })
      return () => {
        subscription.unsubscribe()
        if (!!lastLive) {
          lastLive.unsubscribe()
        }
      }
    })
}