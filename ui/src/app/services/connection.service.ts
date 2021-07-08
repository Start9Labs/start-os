import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge, Subscription } from 'rxjs'
import { ConnectionStatus, PatchDbModel } from './patch-db/patch-db.service'
import { HttpService, Method } from './http.service'
import { distinctUntilChanged } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private readonly networkState$ = new BehaviorSubject<boolean>(true)
  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(ConnectionFailure.None)
  private subs: Subscription[] = []

  constructor (
    private readonly httpService: HttpService,
    private readonly patch: PatchDbModel,
  ) { }

  watchFailure$ () {
    return this.connectionFailure$.asObservable()
  }

  start () {
    this.subs = [
      merge(fromEvent(window, 'online'), fromEvent(window, 'offline'))
      .subscribe(event => {
        this.networkState$.next(event.type === 'online')
      }),

      combineLatest([this.networkState$.pipe(distinctUntilChanged()), this.patch.watchConnection$().pipe(distinctUntilChanged())])
      .subscribe(async ([network, connectionStatus]) => {
        console.log('CONNECTION STATUS', connectionStatus)
        const addrs = this.patch.data['server-info']?.['connection-addresses']
        if (connectionStatus !== ConnectionStatus.Disconnected) {
          this.connectionFailure$.next(ConnectionFailure.None)
        } else if (!network) {
          this.connectionFailure$.next(ConnectionFailure.Network)
        } else {
          console.log('diagnosing')
          this.connectionFailure$.next(ConnectionFailure.Diagnosing)
          const torSuccess = await this.testAddrs(addrs?.tor || [])
          if (torSuccess) {
            console.log('TOR SUCCESS, EMBASSY IS PROBLEM')
            this.connectionFailure$.next(ConnectionFailure.Embassy)
          } else {
            const clearnetSuccess = await this.testAddrs(addrs?.clearnet || [])
            if (clearnetSuccess) {
              console.log('CLEARNET SUCCESS, TOR IS PROBLEM')
              this.connectionFailure$.next(ConnectionFailure.Tor)
            } else {
              console.log('INTERNET IS PROBLEM')
              this.connectionFailure$.next(ConnectionFailure.Internet)
            }
          }
        }
      }),
    ]
  }

  stop () {
    this.subs.forEach(sub => {
      sub.unsubscribe()
    })
    this.subs = []
  }

  private async testAddrs (addrs: string[]): Promise<boolean> {
    if (!addrs.length) return true

    const results = await Promise.all(addrs.map(async addr => {
      try {
        await this.httpService.httpRequest({
          method: Method.GET,
          url: addr,
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
  Internet = 'internet',
}
