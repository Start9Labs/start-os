import { Component } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { QrCodeModule } from 'ng-qrcode'

@Component({
  selector: 'qr',
  template: '<qr-code [value]="context.data" size="350"></qr-code>',
  imports: [QrCodeModule],
})
export class QRModal {
  readonly context = injectContext<TuiDialogContext<void, string>>()
}
