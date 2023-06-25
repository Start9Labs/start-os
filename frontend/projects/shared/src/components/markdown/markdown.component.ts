import { Component, Inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import {
  catchError,
  ignoreElements,
  share,
  defer,
  isObservable,
  Observable,
  of,
} from 'rxjs'

import { getErrorMessage } from '../../services/error-toast.service'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.component.html',
  styleUrls: ['./markdown.component.scss'],
})
export class MarkdownComponent {
  readonly content$ = defer(() =>
    isObservable(this.context.data.content)
      ? this.context.data.content
      : of(this.context.data.content),
  ).pipe(share())

  readonly error$ = this.content$.pipe(
    ignoreElements(),
    catchError(e => of(getErrorMessage(e))),
  )

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<
      void,
      { content: string | Observable<string> }
    >,
  ) {}

  get title(): string {
    return this.context.label || ''
  }
}
