import { Injectable } from '@angular/core'
import { fromEvent, Observable, Subject, Subscription, timer } from 'rxjs'
import { debounceTime, delay, retryWhen, startWith, switchMap, tap } from 'rxjs/operators'
import { ApiService } from './api/api.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  private offlineSubscription: Subscription
  private onlineSubscription: Subscription
  private httpSubscription: Subscription
  private readonly currentState: ConnectionState = {
    network: true,
    internet: true,
  }
  private readonly stateChangeEventEmitter = new Subject<ConnectionState>()

  constructor (
    private readonly apiService: ApiService,
  ) {
    this.checkNetworkState()
    this.checkInternetState()
  }

  ngOnDestroy (): void {
    try {
      this.offlineSubscription.unsubscribe()
      this.onlineSubscription.unsubscribe()
      this.httpSubscription.unsubscribe()
    } catch (e) {
      console.error(e.message)
    }
  }

  /**
   * Monitor Network & Internet connection status by subscribing to this observer.
   */
  monitor$ (): Observable<ConnectionState> {
    return this.stateChangeEventEmitter.pipe(
      debounceTime(300),
      startWith(this.currentState),
    )
  }

  private checkNetworkState (): void {
    this.onlineSubscription = fromEvent(window, 'online').subscribe(() => {
      this.currentState.network = true
      this.checkInternetState()
      this.emitEvent()
    })

    this.offlineSubscription = fromEvent(window, 'offline').subscribe(() => {
      this.currentState.network = true
      this.checkInternetState()
      this.emitEvent()
    })
  }

  private checkInternetState (): void {

    if (this.httpSubscription) {
      this.httpSubscription.unsubscribe()
    }

    // ping server every 10 seconds
    this.httpSubscription = timer(0, 10000)
      .pipe(
        switchMap(() => this.apiService.echo()),
        retryWhen(errors =>
          errors.pipe(
            tap(val => {
              console.error('Echo error: ', val)
              this.currentState.internet = true
              this.emitEvent()
            }),
            // restart after 2 seconds
            delay(2000),
          ),
        ),
      )
      .subscribe(() => {
        this.currentState.internet = true
        this.emitEvent()
      })
  }

  private emitEvent (): void {
    this.stateChangeEventEmitter.next(this.currentState)
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
  internet: boolean
}