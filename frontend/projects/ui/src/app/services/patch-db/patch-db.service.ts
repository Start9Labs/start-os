import { Inject, Injectable, InjectionToken } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { Bootstrapper, PatchDB, Source, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import {
  catchError,
  debounceTime,
  finalize,
  mergeMap,
  skip,
  switchMap,
  take,
  tap,
  withLatestFrom,
} from 'rxjs/operators'
import { isEmptyObject, pauseFor } from '@start9labs/shared'
import { DataModel } from './data-model'
import { ApiService } from '../api/embassy-api.service'
import { AuthService } from '../auth.service'
import { patch } from '@start9labs/emver'

export const PATCH_HTTP = new InjectionToken<Source<DataModel>>('')
export const PATCH_SOURCE = new InjectionToken<Source<DataModel>>('')
export const BOOTSTRAPPER = new InjectionToken<Bootstrapper<DataModel>>('')
export const AUTH = new InjectionToken<AuthService>('')
export const STORAGE = new InjectionToken<Storage>('')

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
  private sources$: BehaviorSubject<Source<DataModel>[]> = new BehaviorSubject([
    this.wsSource,
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
    @Inject(PATCH_SOURCE) private readonly wsSource: Source<DataModel>,
    @Inject(PATCH_SOURCE) private readonly pollSource: Source<DataModel>,
    @Inject(PATCH_HTTP) private readonly http: ApiService,
    @Inject(BOOTSTRAPPER)
    private readonly bootstrapper: Bootstrapper<DataModel>,
    @Inject(AUTH) private readonly auth: AuthService,
    @Inject(STORAGE) private readonly storage: Storage,
  ) {}

  async init(): Promise<void> {
    const cache = await this.bootstrapper.init()
    this.sources$.next([this.wsSource, this.http])

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
    // TODO: refactor with a better solution to race condition
    const argsString = '/' + args.join('/')
    const source$ =
      this.patchDb?.store.watch$(...(args as [])) ||
      this.patchConnection$.pipe(
        skip(1),
        take(1),
        switchMap(() => this.patchDb.store.watch$(...(args as []))),
      )

    console.log('patchDB: WATCHING ', argsString)

    return source$.pipe(
      tap(data => console.log('patchDB: NEW VALUE', argsString, data)),
      catchError(e => {
        console.error('patchDB: WATCH ERROR', e)
        return of(e.message)
      }),
      finalize(() => console.log('patchDB: UNSUBSCRIBING', argsString)),
    )
  }
}
