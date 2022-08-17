import { Injectable } from '@angular/core'
import {
  BehaviorSubject,
  combineLatest,
  fromEvent,
  merge,
  Observable,
  Subject,
} from 'rxjs'
import {
  distinctUntilChanged,
  filter,
  map,
  shareReplay,
  startWith,
  take,
  tap,
} from 'rxjs/operators'
import { ConfigService } from './config.service'

export enum PatchConnection {
  Initializing = 'initializing',
  Connected = 'connected',
  Disconnected = 'disconnected',
}

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private readonly patchDBConnectionError$ = new Subject<any>()
  private readonly patchConnection$ = new BehaviorSubject<PatchConnection>(
    PatchConnection.Initializing,
  )
  readonly patchConnected$ = this.watchPatchConnection$().pipe(
    filter(status => status === PatchConnection.Connected),
    take(1),
    shareReplay(),
  )

  private readonly networkState$ = merge(
    fromEvent(window, 'online').pipe(map(() => true)),
    fromEvent(window, 'offline').pipe(map(() => false)),
  ).pipe(
    startWith(null),
    map(() => navigator.onLine),
  )

  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(
    ConnectionFailure.None,
  )

  constructor(private readonly configService: ConfigService) {}

  watchFailure$() {
    return this.connectionFailure$.asObservable()
  }

  watchDisconnected$() {
    return this.connectionFailure$.pipe(
      map(failure => failure !== ConnectionFailure.None),
    )
  }

  start(): Observable<unknown> {
    return combineLatest([
      // 1
      this.networkState$.pipe(distinctUntilChanged()),
      // 2
      this.watchPatchConnection$().pipe(distinctUntilChanged()),
    ]).pipe(
      tap(([network, patchConnection]) => {
        if (!network) {
          this.connectionFailure$.next(ConnectionFailure.Network)
        } else if (patchConnection !== PatchConnection.Disconnected) {
          this.connectionFailure$.next(ConnectionFailure.None)
        } else if (!this.configService.isTor()) {
          this.connectionFailure$.next(ConnectionFailure.Lan)
        } else {
          this.connectionFailure$.next(ConnectionFailure.Tor)
        }
      }),
    )
  }

  watchPatchConnection$(): Observable<PatchConnection> {
    return this.patchConnection$.asObservable()
  }

  setPatchConnection(status: PatchConnection) {
    this.patchConnection$.next(status)
  }

  setPatchError(e: any) {
    this.patchDBConnectionError$.next(e)
    this.patchConnection$.next(
      e ? PatchConnection.Disconnected : PatchConnection.Connected,
    )
  }
}

export enum ConnectionFailure {
  None = 'none',
  Network = 'network',
  Tor = 'tor',
  Lan = 'lan',
}
