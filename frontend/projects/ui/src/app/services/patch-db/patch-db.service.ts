import { Inject, Injectable } from '@angular/core'
import { Bootstrapper, PatchDB, Store } from 'patch-db-client'
import { BehaviorSubject, Observable, of, Subscription } from 'rxjs'
import {
  catchError,
  debounceTime,
  filter,
  finalize,
  shareReplay,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { DataModel } from './data-model'
import { AuthService } from '../auth.service'
import { BOOTSTRAPPER } from './patch-db.factory'

export enum PatchConnection {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class PatchDbService {
  private readonly patchConnection$ = new BehaviorSubject<PatchConnection>(
    PatchConnection.Initializing,
  )
  private subs: Subscription[] = []

  readonly connected$ = this.watchPatchConnection$().pipe(
    filter(status => status === PatchConnection.Connected),
    take(1),
    shareReplay(),
  )

  constructor(
    @Inject(BOOTSTRAPPER)
    private readonly bootstrapper: Bootstrapper<DataModel>,
    private readonly auth: AuthService,
    private readonly patchDb: PatchDB<DataModel>,
  ) {}

  async start(): Promise<void> {
    this.subs.push(
      // PatchDB Connection Monitoring
      this.patchDb.connectionError$
        .pipe(
          tap(e => {
            console.log('patchDB: ERROR', e)
            if (e) {
              this.patchConnection$.next(PatchConnection.Disconnected)
              // if (e.status === 'unauthenticated') this.auth.setUnverified()
            } else {
              this.patchConnection$.next(PatchConnection.Connected)
            }
          }),
        )
        .subscribe(),

      // Cache Monitoring
      this.patchDb.cache$
        .pipe(
          debounceTime(420),
          tap(cache => {
            this.bootstrapper.update(cache)
          }),
        )
        .subscribe(),
    )
  }

  stop(): void {
    console.log('patchDB: STOPPING')
    this.patchConnection$.next(PatchConnection.Initializing)
    this.patchDb.store.reset()
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
