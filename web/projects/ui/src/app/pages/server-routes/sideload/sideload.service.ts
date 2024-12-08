import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { BehaviorSubject, endWith, shareReplay, Subject, switchMap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()

  readonly websocketConnected$ = new BehaviorSubject(false)

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api
        .openWebsocket$<T.FullProgress>(guid, {
          openObserver: {
            next: () => this.websocketConnected$.next(true),
          },
          closeObserver: {
            next: () => this.websocketConnected$.next(false),
          },
        })
        .pipe(endWith(null)),
    ),
    shareReplay(1),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
