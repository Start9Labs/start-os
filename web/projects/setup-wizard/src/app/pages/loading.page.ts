import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { Router } from '@angular/router'
import { ErrorService, InitializingComponent } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  EMPTY,
  filter,
  from,
  interval,
  map,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: '<app-initializing [setupType]="type" [progress]="progress()" />',
  imports: [InitializingComponent],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class LoadingPage {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  readonly type = inject(StateService).setupType
  readonly router = inject(Router)
  readonly progress = toSignal(
    from(this.getStatus()).pipe(
      filter(Boolean),
      take(1),
      switchMap(({ guid, progress }) =>
        this.api.openProgressWebsocket$(guid).pipe(
          startWith(progress),
          catchError((_, watch$) =>
            interval(2000).pipe(
              switchMap(() =>
                from(this.api.getStatus()).pipe(catchError(() => EMPTY)),
              ),
              take(1),
              switchMap(() => watch$),
            ),
          ),
          tap(({ overall }) => {
            if (overall === true) {
              this.getStatus()
            }
          }),
        ),
      ),
      map(({ phases, overall }) => ({
        total: getDecimal(overall),
        message: phases
          .filter(p => p.progress !== true && p.progress !== null)
          .map(p => `${p.name}${getPhaseBytes(p.progress)}`)
          .join(','),
      })),
      catchError(e => {
        this.errorService.handleError(e)
        return EMPTY
      }),
    ),
    { initialValue: { total: 0, message: '' } },
  )

  private async getStatus(): Promise<{
    status: 'running'
    guid: string
    progress: T.FullProgress
  } | null> {
    const res = await this.api.getStatus()

    if (!res) {
      this.router.navigate(['home'])
      return null
    } else if (res.status === 'complete') {
      this.router.navigate(['success'])
      return null
    } else {
      return res
    }
  }
}

function getDecimal(progress: T.Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.total && progress.done / progress.total
  }
}

function getPhaseBytes(progress: T.Progress): string {
  return progress === true || !progress
    ? ''
    : `: (${progress.done}/${progress.total})`
}
