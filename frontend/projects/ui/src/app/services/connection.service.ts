import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge, Subscription } from 'rxjs'
import { PatchConnection, PatchDbService } from './patch-db/patch-db.service'
import { distinctUntilChanged } from 'rxjs/operators'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private readonly networkState$ = new BehaviorSubject<boolean>(true)
  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(ConnectionFailure.None)

  constructor (
    private readonly configService: ConfigService,
    private readonly patch: PatchDbService,
  ) { }

  watchFailure$ () {
    return this.connectionFailure$.asObservable()
  }

  start (): Subscription[] {
    const sub1 = merge(fromEvent(window, 'online'), fromEvent(window, 'offline'))
    .subscribe(event => {
      this.networkState$.next(event.type === 'online')
    })

    const sub2 = combineLatest([
      // 1
      this.networkState$
      .pipe(
        distinctUntilChanged(),
      ),
      // 2
      this.patch.watchPatchConnection$()
      .pipe(
        distinctUntilChanged(),
      ),
      // 3
      this.patch.watch$('server-info', 'update-progress')
      .pipe(
        distinctUntilChanged(),
      ),
    ])
    .subscribe(async ([network, patchConnection, progress]) => {
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
    })
    return [sub1, sub2]
  }
}

export enum ConnectionFailure {
  None = 'none',
  Network = 'network',
  Tor = 'tor',
  Lan = 'lan',
}
