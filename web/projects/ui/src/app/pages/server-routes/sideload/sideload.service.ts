import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { endWith, ReplaySubject, shareReplay, Subject, switchMap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { retryWithState } from 'src/app/util/retry-with-state'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()

  readonly websocketConnected$ = new ReplaySubject()

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api
        .openWebsocket$<T.FullProgress>(guid, {
          openObserver: {
            next: () => this.websocketConnected$.next(''),
          },
        })
        .pipe(endWith(null)),
    ),
    shareReplay(1),
    // @TODO Matt what would happen if the sideloading finished while we waited?
    retryWithState(),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
