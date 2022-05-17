import { Inject, Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import {
  BehaviorSubject,
  Observable,
  of,
  ReplaySubject,
  Subscription,
} from 'rxjs'
import {
  catchError,
  debounceTime,
  filter,
  finalize,
  mergeMap,
  switchMap,
  take,
  tap,
  withLatestFrom,
} from 'rxjs/operators'
import { isEmptyObject, pauseFor } from '@start9labs/shared'
import { DataModel } from './data-model'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { BOOTSTRAPPER, PATCH_SOURCE } from './patch-db.factory'

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
  private patchConnection$ = new ReplaySubject<PatchConnection>(1)
  private wsSuccess$ = new BehaviorSubject(false)
  private polling$ = new BehaviorSubject(false)
  private patchDb: PatchDB<DataModel>
  private subs: Subscription[] = []
  private sources$: BehaviorSubject<Source<DataModel>[]> = new BehaviorSubject([
    this.sources[0],
  ])

  data: DataModel
  errors = 0

  getData() {
    return this.patchDb.store.cache.data
  }

  // TODO: Refactor to use `Observable` so that we can react to PatchDb becoming loaded
  get loaded(): boolean {
    return (
      this.patchDb?.store?.cache?.data &&
      !isEmptyObject(this.patchDb.store.cache.data)
    )
  }

  constructor(
    // [wsSources, pollSources]
    @Inject(PATCH_SOURCE) private readonly sources: Source<DataModel>[],
    @Inject(BOOTSTRAPPER)
    private readonly bootstrapper: Bootstrapper<DataModel>,
    private readonly http: ApiService,
    private readonly auth: AuthService,
    private readonly storage: Storage,
  ) {}

  async init(): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.sources$.next([this.sources[0], this.http])

    this.patchDb = new PatchDB(this.sources$, this.http, cache)

    this.patchConnection$.next(PatchConnection.Initializing)
    this.data = this.patchDb.store.cache.data
  }

  async start(): Promise<void> {
    await this.init()

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
              this.sources$.next([this.sources[1], this.http])
              return
            }

            console.log('patchDB: WEBSOCKET FAILED', e)
            this.polling$.next(true)
            this.sources$.next([this.sources[1], this.http])
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
              console.log(
                polling
                  ? 'patchDB: POLL CONNECTED'
                  : 'patchDB: WEBSOCKET CONNECTED',
              )
              this.patchConnection$.next(PatchConnection.Connected)
              if (!wsSuccess && !polling) {
                console.log('patchDB: WEBSOCKET SUCCESS')
                this.storage.set(this.WS_SUCCESS, 'true')
                this.wsSuccess$.next(true)
              }
            } else if (
              connection === PatchConnection.Disconnected &&
              wsSuccess
            ) {
              console.log('patchDB: SWITCHING BACK TO WEBSOCKETS')
              this.patchConnection$.next(PatchConnection.Initializing)
              this.polling$.next(false)
              this.sources$.next([this.sources[0], this.http])
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

  stop(): void {
    if (this.patchDb) {
      console.log('patchDB: STOPPING')
      this.patchConnection$.next(PatchConnection.Initializing)
      this.patchDb.store.reset()
    }
    this.subs.forEach(x => x.unsubscribe())
    this.subs = []
  }

  watchPatchConnection$(): Observable<PatchConnection> {
    return this.patchConnection$.asObservable()
  }

  // prettier-ignore
  watch$: Store<DataModel>['watch$'] = (...args: (string | number)[]): Observable<DataModel> => {
    const argsString = '/' + args.join('/')

    console.log('patchDB: WATCHING ', argsString)

    return this.patchConnection$.pipe(
      filter(status => status === PatchConnection.Connected),
      take(1),
      switchMap(() => this.patchDb.store.watch$(...(args as []))),
      tap(data => console.log('patchDB: NEW VALUE', argsString, data)),
      catchError(e => {
        console.error('patchDB: WATCH ERROR', e)
        return of(e.message)
      }),
      finalize(() => console.log('patchDB: UNSUBSCRIBING', argsString)),
    )
  }
}
