import { Injectable } from '@angular/core'
import { combineLatest, fromEvent, merge, ReplaySubject } from 'rxjs'
import { distinctUntilChanged, map, startWith } from 'rxjs/operators'

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
  readonly websocketConnected$ = new ReplaySubject<boolean>(1)
  readonly connected$ = combineLatest([
    this.networkConnected$,
    this.websocketConnected$.pipe(distinctUntilChanged()),
  ]).pipe(
    map(([network, websocket]) => network && websocket),
    distinctUntilChanged(),
  )
}
