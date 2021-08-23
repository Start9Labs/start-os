import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge, Subscription } from 'rxjs'
import { PatchConnection, PatchDbService } from './patch-db/patch-db.service'
import { HttpService, Method } from './http.service'
import { distinctUntilChanged } from 'rxjs/operators'
import { ConfigService } from './config.service'
import { pauseFor } from '../util/misc.util'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private readonly networkState$ = new BehaviorSubject<boolean>(true)
  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(ConnectionFailure.None)

  constructor (
    private readonly httpService: HttpService,
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
      this.patch.watch$('server-info', 'connection-addresses')
      .pipe(
        distinctUntilChanged(),
      ),
    ])
    .subscribe(async ([network, patchConnection, addrs]) => {
      if (patchConnection !== PatchConnection.Disconnected) {
        this.connectionFailure$.next(ConnectionFailure.None)
      } else if (!network) {
        this.connectionFailure$.next(ConnectionFailure.Network)
      } else if (!this.configService.isTor()) {
        this.connectionFailure$.next(ConnectionFailure.Lan)
      } else {
        // diagnosing
        this.connectionFailure$.next(ConnectionFailure.Diagnosing)
        const torSuccess = await this.testAddrs(addrs.tor)
        if (torSuccess) {
          // TOR SUCCESS, EMBASSY IS PROBLEM
          this.connectionFailure$.next(ConnectionFailure.Embassy)
        } else {
          const clearnetSuccess = await this.testAddrs(addrs.clearnet)
          if (clearnetSuccess) {
            // CLEARNET SUCCESS, TOR IS PROBLEM
            this.connectionFailure$.next(ConnectionFailure.Tor)
          } else {
            // INTERNET IS PROBLEM
            this.connectionFailure$.next(ConnectionFailure.Internet)
          }
        }
      }
    })
    return [sub1, sub2]
  }

  private async testAddrs (addrs: string[]): Promise<boolean> {
    if (!addrs.length) return true

    const results = await Promise.all(addrs.map(async addr => {
      try {
        await this.httpService.httpRequest({
          method: Method.GET,
          url: addr,
          withCredentials: false,
        })
        return true
      } catch (e) {
        return false
      }
    }))
    return results.includes(true)
  }
}

export enum ConnectionFailure {
  None = 'none',
  Diagnosing = 'diagnosing',
  Network = 'network',
  Embassy = 'embassy',
  Tor = 'tor',
  Lan = 'lan',
  Internet = 'internet',
}
