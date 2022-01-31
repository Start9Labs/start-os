import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { QRComponent } from './qr.component'
import { IonicModule } from '@ionic/angular'
import { QrCodeModule } from 'ng-qrcode'

@NgModule({
  declarations: [
    QRComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    QrCodeModule,
  ],
  exports: [QRComponent],
})
export class QRComponentModule { }
