import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge, Subject } from 'rxjs'
import {
  distinctUntilChanged,
  map,
  shareReplay,
  startWith,
} from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService {
  readonly networkConnected$ = merge(
    fromEvent(window, 'online'),
    fromEvent(window, 'offline'),
  ).pipe(
    startWith(null),
    map(() => navigator.onLine),
    distinctUntilChanged(),
  )
  readonly websocketConnected$ = new Subject<boolean>()
  readonly connected$ = combineLatest([
    this.networkConnected$,
    this.websocketConnected$.pipe(shareReplay(1)),
  ]).pipe(map(([network, websocket]) => network && websocket))
}
