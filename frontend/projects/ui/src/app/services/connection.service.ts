import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest, fromEvent, merge } from 'rxjs'
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
  readonly websocketConnected$ = new BehaviorSubject(false)
  readonly connected$ = combineLatest([
    this.networkConnected$,
    this.websocketConnected$,
  ]).pipe(map(([network, websocket]) => network && websocket))
}
