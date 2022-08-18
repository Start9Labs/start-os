import { Injectable } from '@angular/core'
import {
  BehaviorSubject,
  combineLatest,
  fromEvent,
  merge,
  Observable,
  Subject,
} from 'rxjs'
import { distinctUntilChanged, map, tap } from 'rxjs/operators'
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
  private readonly networkState$ = merge(
    fromEvent(window, 'online'),
    fromEvent(window, 'offline'),
  ).pipe(
    map(() => navigator.onLine),
    distinctUntilChanged(),
  )
  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(
    ConnectionFailure.None,
  )

  readonly patchConnected$ = this.patchConnection$.pipe(
    map(status => status === PatchConnection.Connected),
  )

  readonly watchFailure$ = this.connectionFailure$.pipe(distinctUntilChanged())

  readonly watchDisconnected$ = this.connectionFailure$.pipe(
    map(failure => failure !== ConnectionFailure.None),
    distinctUntilChanged(),
  )

  constructor(private readonly configService: ConfigService) {}

  start(): Observable<unknown> {
    return combineLatest([
      // 1
      this.networkState$,
      // 2
      this.patchConnection$,
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
