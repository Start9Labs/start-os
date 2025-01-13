import { inject, Injectable } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  EMPTY,
  endWith,
  shareReplay,
  Subject,
  switchMap,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api
        .openWebsocket$<T.FullProgress>(guid, {
          closeObserver: {
            next: event => {
              if (event.code !== 1000) {
                this.errorService.handleError(event.reason)
              }
            },
          },
        })
        .pipe(
          tap(p => {
            if (p.overall === true) {
              this.router.navigate([''], { replaceUrl: true })
            }
          }),
          endWith(null),
        ),
    ),
    catchError(e => {
      this.errorService.handleError('Websocket connection broken. Try again.')
      return EMPTY
    }),
    shareReplay(1),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
