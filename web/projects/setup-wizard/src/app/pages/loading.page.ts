import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { Router } from '@angular/router'
import {
  ErrorService,
  formatProgress,
  InitializingComponent,
} from '@start9labs/shared'
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
  styles: ':host { max-width: unset; align-items: stretch; }',
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
        this.api.openWebsocket$<T.FullProgress>(guid).pipe(
          startWith(progress),
          catchError((_, watch$) =>
            interval(2000).pipe(
              switchMap(() => from(this.api.getStatus())),
              catchError(() => EMPTY),
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
      map(formatProgress),
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
