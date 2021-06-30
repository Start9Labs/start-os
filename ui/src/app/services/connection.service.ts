import { Injectable } from '@angular/core'
import { BehaviorSubject, fromEvent, merge, Observable, Subscription, timer } from 'rxjs'
import { delay, retryWhen, switchMap, tap } from 'rxjs/operators'
import { ApiService } from './api/api.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private httpSubscription$: Subscription
  private readonly networkState$ = new BehaviorSubject<boolean>(navigator.onLine)
  private readonly internetState$ = new BehaviorSubject<boolean | null>(null)

  constructor (
    private readonly apiService: ApiService,
  ) {
    merge(fromEvent(window, 'online'), fromEvent(window, 'offline'))
    .subscribe(event => {
      this.networkState$.next(event.type === 'online')
    })

    this.networkState$
    .subscribe(online => {
      if (online) {
        this.testInternet()
      } else {
        this.killHttp()
        this.internetState$.next(false)
      }
    })
  }

  monitor$ (): Observable<boolean> {
    return this.internetState$.asObservable()
  }

  private testInternet (): void {
    this.killHttp()

    // ping server every 10 seconds
    this.httpSubscription$ = timer(0, 10000)
      .pipe(
        switchMap(() => this.apiService.echo()),
        retryWhen(errors =>
          errors.pipe(
            tap(val => {
              console.error('Echo error: ', val)
              this.internetState$.next(false)
            }),
            // restart after 2 seconds
            delay(2000),
          ),
        ),
      )
      .subscribe(() => {
        this.internetState$.next(true)
      })
  }

  private killHttp () {
    if (this.httpSubscription$) {
      this.httpSubscription$.unsubscribe()
      this.httpSubscription$ = undefined
    }
  }
}

/**
 * Instance of this interface is used to report current connection status.
 */
 export interface ConnectionState {
  /**
   * "True" if browser has network connection. Determined by Window objects "online" / "offline" events.
   */
  network: boolean
  /**
   * "True" if browser has Internet access. Determined by heartbeat system which periodically makes request to heartbeat Url.
   */
  internet: boolean | null
}