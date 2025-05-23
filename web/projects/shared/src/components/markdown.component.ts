import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute } from '@angular/router'
import { TuiDialogContext, TuiLoader, TuiNotification } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { catchError, ignoreElements, Observable, of } from 'rxjs'
import { SafeLinksDirective } from '../directives/safe-links.directive'
import { MarkdownPipe } from '../pipes/markdown.pipe'
import { getErrorMessage } from '../services/error.service'

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
      <tui-loader textContent="Loading" [style.height.%]="100" />
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: { class: 'g-subpage' },
  imports: [
    TuiNotification,
    TuiLoader,
    NgDompurifyModule,
    MarkdownPipe,
    SafeLinksDirective,
  ],
})
export class MarkdownComponent {
  private readonly data =
    injectContext<TuiDialogContext<void, { content: Observable<string> }>>({
      optional: true,
    })?.data || inject(ActivatedRoute).snapshot.data

  readonly content = toSignal<string>(this.data['content'])
  readonly error = toSignal(
    this.data['content'].pipe(
      ignoreElements(),
      catchError(e => of(getErrorMessage(e))),
    ),
  )
}

export const MARKDOWN = new PolymorpheusComponent(MarkdownComponent)
