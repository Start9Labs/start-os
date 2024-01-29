import { Directive, inject, Output } from '@angular/core'
import { IntersectionObserveeService } from '@ng-web-apis/intersection-observer'
import { ErrorService } from '@start9labs/shared'
import {
  catchError,
  defer,
  filter,
  from,
  map,
  of,
  scan,
  switchMap,
  tap,
} from 'rxjs'
import { LogsComponent } from './logs.component'
import { convertAnsi } from '../../utils/convert-ansi'

@Directive({
  standalone: true,
  selector: '[logsFetch]',
  providers: [IntersectionObserveeService],
})
export class LogsFetchDirective {
  private readonly observer = inject(IntersectionObserveeService)
  private readonly component = inject(LogsComponent)
  private readonly errors = inject(ErrorService)

  @Output()
  readonly logsFetch = defer(() => this.observer).pipe(
    filter(([{ isIntersecting }]) => isIntersecting && !this.component.scroll),
    switchMap(() =>
      from(
        this.component.fetchLogs({
          cursor: this.component.startCursor,
          before: true,
          limit: 400,
        }),
      ),
    ),
    tap(res => this.component.setCursor(res['start-cursor'])),
    map(({ entries }) => convertAnsi(entries)),
    catchError(e => {
      this.errors.handleError(e)

      return of('')
    }),
    filter(Boolean),
    scan((acc: readonly string[], value) => [value, ...acc], []),
  )
}
