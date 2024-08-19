import { Injectable } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  EMPTY,
  endWith,
  ReplaySubject,
  Subject,
  switchMap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

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
        .pipe(
          catchError(err => {
            this.errorService.handleError(err)
            return EMPTY
          }),
          endWith(null),
        ),
    ),
  )

  constructor(
    private readonly api: ApiService,
    private readonly errorService: ErrorService,
  ) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
