import { inject, Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  defer,
  from,
  map,
  Observable,
  startWith,
  switchMap,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { StateService } from 'src/app/services/state.service'

interface MappedProgress {
  readonly total: number | null
  readonly message: string
}

@Injectable({ providedIn: 'root' })
export class InitService extends Observable<MappedProgress> {
  private readonly state = inject(StateService)
  private readonly api = inject(ApiService)
  private readonly progress$ = defer(() =>
    from(this.api.initGetProgress()),
  ).pipe(
    switchMap(({ guid, progress }) =>
      this.api
        .openWebsocket$<T.FullProgress>(guid, {
          closeObserver: {
            next: () => {
              this.state.syncState()
            },
          },
        })
        .pipe(startWith(progress)),
    ),
    map(({ phases, overall }) => ({
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
        .map(p => `<b>${p.name}</b>${getPhaseBytes(p.progress)}`)
        .join(', '),
    })),
    tap(({ total }) => {
      if (total === 1) {
        this.state.syncState()
      }
    }),
    catchError((e, caught$) => {
      console.error(e)
      this.state.syncState()
      return caught$
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
  return progress === false ? '' : `: (${progress.done}/${progress.total})`
}
