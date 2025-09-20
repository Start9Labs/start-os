import { Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  formatProgress,
  InitializingComponent,
  provideSetupLogsService,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  catchError,
  defer,
  from,
  map,
  startWith,
  switchMap,
  tap,
  timer,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  template: `
    <app-initializing [progress]="progress()" />
  `,
  providers: [provideSetupLogsService(ApiService)],
  styles: ':host { height: 100%; }',
  imports: [InitializingComponent],
})
export default class InitializingPage {
  private readonly api = inject(ApiService)
  private readonly state = inject(StateService)

  readonly progress = toSignal(
    defer(() => from(this.api.initFollowProgress())).pipe(
      switchMap(({ guid, progress }) =>
        this.api
          .openWebsocket$<T.FullProgress>(guid, {
            closeObserver: {
              next: () => {
                this.state.retrigger(true)
              },
            },
          })
          .pipe(startWith(progress)),
      ),
      map(formatProgress),
      tap(({ total }) => {
        if (total === 1) {
          this.state.retrigger(true)
        }
      }),
      catchError((_, caught$) => {
        this.state.retrigger(true)
        return timer(500).pipe(switchMap(() => caught$))
      }),
    ),
    { initialValue: { total: 0, message: 'waiting...' } },
  )
}
