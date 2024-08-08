import { inject, Injectable } from '@angular/core'
import { combineLatest, Observable, shareReplay } from 'rxjs'
import { distinctUntilChanged, map } from 'rxjs/operators'
import { NetworkService } from 'src/app/services/network.service'
import { StateService } from 'src/app/services/state.service'

@Injectable({
  providedIn: 'root',
})
export class ConnectionService extends Observable<boolean> {
  private readonly stream$ = combineLatest([
    inject(NetworkService),
    inject(StateService).pipe(map(Boolean)),
  ]).pipe(
    map(([network, websocket]) => network && websocket),
    distinctUntilChanged(),
    shareReplay(1),
  )

  constructor() {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
