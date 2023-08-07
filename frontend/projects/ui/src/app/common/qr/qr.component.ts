import { Component, Inject } from '@angular/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'

@Component({
  selector: 'qr',
  template: '<qr-code [value]="context.data" size="400"></qr-code>',
})
export class QRComponent {
  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    readonly context: TuiDialogContext<void, string>,
  ) {}
}
