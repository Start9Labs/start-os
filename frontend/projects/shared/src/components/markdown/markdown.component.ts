import { Component, Input, Optional } from '@angular/core'
import { ModalController } from '@ionic/angular'

import { getErrorMessage } from '../../services/error-toast.service'
import { AbstractApiService } from '../../services/api.service'
import { defer, from, isObservable, Observable, of } from 'rxjs'
import { catchError, ignoreElements, share } from 'rxjs/operators'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.component.html',
  styleUrls: ['./markdown.component.scss'],
})
export class MarkdownComponent {
  @Input() contentUrl?: string
  @Input() content?: string | Observable<string>
  @Input() title = ''

  private readonly data$ = defer(() => this.contentObservable).pipe(share())

  readonly error$ = this.data$.pipe(
    ignoreElements(),
    catchError(e => of(getErrorMessage(e))),
  )

  readonly content$ = this.data$.pipe(catchError(() => of([])))

  constructor(
    private readonly modalCtrl: ModalController,
    @Optional()
    private readonly embassyApi: AbstractApiService | null,
  ) {}

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }

  private get contentObservable() {
    if (isObservable(this.content)) {
      return this.content
    }

    return this.contentUrl
      ? from(this.embassyApi?.getStatic(this.contentUrl))
      : of(this.content)
  }
}
