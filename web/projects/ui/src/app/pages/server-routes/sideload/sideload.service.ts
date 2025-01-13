import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { shareReplay, Subject, switchMap } from 'rxjs'
import { WebSocketSubject } from 'rxjs/webSocket'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()

  readonly progress$ = this.guid$.pipe(
    switchMap(guid => {
      const wsSubject: WebSocketSubject<T.FullProgress | null> =
        this.api.openWebsocket$<T.FullProgress | null>(guid, {
          closingObserver: {
            next: () => wsSubject.next(null),
          },
        })

      return wsSubject
    }),
    shareReplay(1),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
