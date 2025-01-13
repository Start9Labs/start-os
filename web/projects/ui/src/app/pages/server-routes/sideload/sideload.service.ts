import { inject, Injectable } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  EMPTY,
  endWith,
  shareReplay,
  Subject,
  switchMap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()
  private readonly errorService = inject(ErrorService)

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api.openWebsocket$<T.FullProgress>(guid).pipe(endWith(null)),
    ),
    catchError(e => {
      this.errorService.handleError(e)
      return EMPTY
    }),
    shareReplay(1),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
