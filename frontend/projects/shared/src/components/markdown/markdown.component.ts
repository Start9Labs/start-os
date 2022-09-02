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

  readonly content$ = defer(() =>
    isObservable(this.content) ? this.content : of(this.content),
  ).pipe(share())

  readonly error$ = this.content$.pipe(
    ignoreElements(),
    catchError(e => of(getErrorMessage(e))),
  )

  constructor(private readonly modalCtrl: ModalController) {}

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }
}
