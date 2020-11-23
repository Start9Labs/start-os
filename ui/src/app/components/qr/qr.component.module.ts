import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { QRComponent } from './qr.component'
import { IonicModule } from '@ionic/angular'
import { QRCodeModule } from 'angularx-qrcode'

@NgModule({
  declarations: [
    QRComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    QRCodeModule,
  ],
  exports: [QRComponent],
})
export class QRComponentModule { }
