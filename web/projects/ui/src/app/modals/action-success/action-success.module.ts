import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ActionSuccessPage } from './action-success.page'
import { QrCodeModule } from 'ng-qrcode'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'

@NgModule({
  declarations: [ActionSuccessPage],
  imports: [CommonModule, IonicModule, QrCodeModule, QRComponentModule],
  exports: [ActionSuccessPage],
})
export class ActionSuccessPageModule {}
