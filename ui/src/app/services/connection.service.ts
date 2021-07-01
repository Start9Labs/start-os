import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge, Observable, Subscription } from 'rxjs'
import { ConnectionStatus } from '../../../../../patch-db/client/dist'
import { DataModel } from '../models/patch-db/data-model'
import { PatchDbModel } from '../models/patch-db/patch-db-model'
import { HttpService, Method } from './http.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private addrs: DataModel['server-info']['connection-addresses']
  private readonly networkState$ = new BehaviorSubject<boolean>(true)
  private readonly connectionFailure$ = new BehaviorSubject<ConnectionFailure>(ConnectionFailure.None)
  private subs: Subscription[] = []

  constructor (
    private readonly httpService: HttpService,
    private readonly patch: PatchDbModel,
  ) { }

  watch$ () {
    return this.connectionFailure$.asObservable()
  }

  start () {
    this.subs = [
      this.patch.watch$('server-info')
      .subscribe(data => {
        if (!data) return
        this.addrs = data['connection-addresses'] || {
          tor: [],
          clearnet: [],
        }
      }),

      merge(fromEvent(window, 'online'), fromEvent(window, 'offline'))
      .subscribe(event => {
        this.networkState$.next(event.type === 'online')
      }),

      combineLatest([this.networkState$, this.patch.connectionStatus$()])
      .subscribe(async ([network, connectionStatus]) => {
        if (connectionStatus !== ConnectionStatus.Disconnected) {
          this.connectionFailure$.next(ConnectionFailure.None)
        } else if (!network) {
          this.connectionFailure$.next(ConnectionFailure.Network)
        } else {
          this.connectionFailure$.next(ConnectionFailure.Diagnosing)
          const torSuccess = await this.testAddrs(this.addrs.tor)
          if (torSuccess) {
            this.connectionFailure$.next(ConnectionFailure.Embassy)
          } else {
            const clearnetSuccess = await this.testAddrs(this.addrs.clearnet)
            if (clearnetSuccess) {
              this.connectionFailure$.next(ConnectionFailure.Tor)
            } else {
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
