import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { defer, isObservable, Observable, of } from 'rxjs'
import { catchError, ignoreElements, share } from 'rxjs/operators'

import { getErrorMessage } from '../../services/error-toast.service'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.component.html',
  styleUrls: ['./markdown.component.scss'],
})
export class MarkdownComponent {
  @Input() content!: string | Observable<string>
  @Input() title!: string

  private readonly data$ = defer(() =>
    isObservable(this.content) ? this.content : of(this.content),
  ).pipe(share())

  readonly error$ = this.data$.pipe(
    ignoreElements(),
    catchError(e => of(getErrorMessage(e))),
  )

  readonly content$ = this.data$.pipe(catchError(() => of([])))

  constructor(private readonly modalCtrl: ModalController) {}

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }
}
