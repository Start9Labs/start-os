import { Injectable } from '@angular/core'
import {
  BehaviorSubject,
  combineLatest,
  fromEvent,
  merge,
  Observable,
  Subject,
  Subscription,
} from 'rxjs'
import { PatchConnection, PatchDbService } from './patch-db/patch-db.service'
import {
  distinctUntilChanged,
  map,
  mapTo,
  startWith,
  tap,
} from 'rxjs/operators'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private readonly networkState$ = merge(
    fromEvent(window, 'online').pipe(mapTo(true)),
    fromEvent(window, 'offline').pipe(mapTo(false)),
  ).pipe(
    startWith(null),
    map(() => navigator.onLine),
  )

  private readonly connectionFailure$ = new Subject<ConnectionFailure>()

  constructor(
    private readonly configService: ConfigService,
    private readonly patch: PatchDbService,
  ) {}

  watchFailure$() {
    return this.connectionFailure$.asObservable()
  }

  start(): Observable<unknown> {
    return combineLatest([
      // 1
      this.networkState$.pipe(distinctUntilChanged()),
      // 2
      this.patch.watchPatchConnection$().pipe(distinctUntilChanged()),
      // 3
      this.patch
        .watch$('server-info', 'status-info', 'update-progress')
        .pipe(distinctUntilChanged()),
    ]).pipe(
      tap(([network, patchConnection, progress]) => {
        if (!network) {
          this.connectionFailure$.next(ConnectionFailure.Network)
        } else if (patchConnection !== PatchConnection.Disconnected) {
          this.connectionFailure$.next(ConnectionFailure.None)
        } else if (!!progress && progress.downloaded === progress.size) {
          this.connectionFailure$.next(ConnectionFailure.None)
        } else if (!this.configService.isTor()) {
          this.connectionFailure$.next(ConnectionFailure.Lan)
        } else {
          this.connectionFailure$.next(ConnectionFailure.Tor)
        }
      }),
    )
  }
}

export enum ConnectionFailure {
  None = 'none',
  Network = 'network',
  Tor = 'tor',
  Lan = 'lan',
}
