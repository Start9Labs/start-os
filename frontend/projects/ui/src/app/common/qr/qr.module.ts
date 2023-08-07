import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { QrCodeModule } from 'ng-qrcode'

import { QRComponent } from './qr.component'

@NgModule({
  declarations: [QRComponent],
  imports: [CommonModule, QrCodeModule],
  exports: [QRComponent],
})
export class QRComponentModule {}
