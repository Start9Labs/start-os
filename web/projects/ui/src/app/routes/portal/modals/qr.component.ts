import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@taiga-ui/polymorpheus'
import { QrCodeModule } from 'ng-qrcode'

@Component({
  standalone: true,
  selector: 'qr',
  template: '<qr-code [value]="context.data" size="400"></qr-code>',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [QrCodeModule],
})
export class QRModal {
  readonly context =
    inject<TuiDialogContext<void, string>>(POLYMORPHEUS_CONTEXT)
}
