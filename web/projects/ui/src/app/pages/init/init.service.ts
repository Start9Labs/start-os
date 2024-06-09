import { inject, Injectable } from '@angular/core'
import { ErrorToastService } from '@start9labs/shared'
import {
  catchError,
  EMPTY,
  exhaustMap,
  filter,
  from,
  interval,
  map,
  Observable,
  shareReplay,
  takeWhile,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({ providedIn: 'root' })
export class InitService extends Observable<number> {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorToastService)
  private readonly progress$ = interval(500).pipe(
    exhaustMap(() =>
      from(this.api.initGetProgress()).pipe(
        catchError(e => {
          this.errorService.present(e)

          return EMPTY
        }),
      ),
    ),
    filter(Boolean),
    map(({ progress: { overall } }) => {
      if (overall === true) {
        return 1
      }

      return overall && overall.done
        ? overall.done / (overall.total || overall.done)
        : 0
    }),
    takeWhile(value => value !== 1, true),
    shareReplay(1),
  )

  constructor() {
    super(subscriber => this.progress$.subscribe(subscriber))
  }
}
