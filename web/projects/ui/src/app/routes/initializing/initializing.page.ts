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
  EMPTY,
  from,
  map,
  startWith,
  switchMap,
  tap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <app-initializing [progress]="progress()" />
  `,
  providers: [provideSetupLogsService(ApiService)],
  styles: ':host { padding: 1rem; }',
  imports: [InitializingComponent],
})
export default class InitializingPage {
  private readonly api = inject(ApiService)
  private readonly state = inject(StateService)

  readonly progress = toSignal(
    defer(() => from(this.api.initFollowProgress())).pipe(
      switchMap(({ guid, progress }) =>
        this.api
          .openWebsocket$<T.FullProgress>(guid, {})
          .pipe(startWith(progress)),
      ),
      map(formatProgress),
      tap<{ total: number; message: string }>(({ total }) => {
        if (total === 1) {
          this.state.syncState()
        }
      }),
      catchError(e => {
        console.error(e)
        return EMPTY
      }),
    ),
    { initialValue: { total: 0, message: '' } },
  )
}
