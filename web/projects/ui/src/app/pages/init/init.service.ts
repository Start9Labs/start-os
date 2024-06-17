import { inject, Injectable } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorToastService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  defer,
  EMPTY,
  from,
  map,
  Observable,
  startWith,
  switchMap,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

interface MappedProgress {
  readonly total: number | null
  readonly message: string
}

@Injectable({ providedIn: 'root' })
export class InitService extends Observable<MappedProgress> {
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorToastService)
  private readonly progress$ = defer(() =>
    from(this.api.initGetProgress()),
  ).pipe(
    switchMap(({ guid, progress }) =>
      this.api
        .openWebsocket$<T.FullProgress>(guid, {})
        .pipe(startWith(progress)),
    ),
    map(({ phases, overall }) => {
      return {
        total: getOverallDecimal(overall),
        message: phases
          .filter(
            (
              p,
            ): p is {
              name: string
              progress: {
                done: number
                total: number | null
              }
            } => p.progress !== true && p.progress !== null,
          )
          .map(p => `${p.name}: (${getPhaseBytes(p.progress)})`)
          .join(','),
      }
    }),
    tap(({ total }) => {
      if (total === 1) this.router.navigate([''])
    }),
    catchError(e => {
      this.errorService.present(e)

      return EMPTY
    }),
  )

  constructor() {
    super(subscriber => this.progress$.subscribe(subscriber))
  }
}

function getOverallDecimal(progress: T.Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.total && progress.done / progress.total
  }
}

function getPhaseBytes(
  progress:
    | false
    | {
        done: number
        total: number | null
      },
): string {
  return progress === false
    ? 'unknown'
    : `${progress.done} of ${progress.total}`
}
