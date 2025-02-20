import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute } from '@angular/router'
import {
  getErrorMessage,
  MarkdownPipeModule,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiLoader, TuiNotification } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { catchError, ignoreElements, of } from 'rxjs'

@Component({
  template: `
    @if (error()) {
      <tui-notification appearance="negative" safeLinks>
        {{ error() }}
      </tui-notification>
    }

    @if (content(); as result) {
      <div safeLinks [innerHTML]="result | markdown | dompurify"></div>
    } @else {
      <tui-loader textContent="Loading" />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: { class: 'g-subpage' },
  imports: [
    TuiNotification,
    TuiLoader,
    MarkdownPipeModule,
    NgDompurifyModule,
    SafeLinksDirective,
  ],
})
export default class ServiceMarkdownRoute {
  private readonly data = inject(ActivatedRoute).snapshot.data

  readonly content = toSignal<string>(this.data['content'])
  readonly error = toSignal(
    this.data['content'].pipe(
      ignoreElements(),
      catchError(e => of(getErrorMessage(e))),
    ),
  )
}
