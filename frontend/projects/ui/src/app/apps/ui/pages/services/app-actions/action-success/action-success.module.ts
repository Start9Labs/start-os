import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { QrCodeModule } from 'ng-qrcode'

import { ActionSuccessPage } from './action-success.page'

@NgModule({
  declarations: [ActionSuccessPage],
  imports: [CommonModule, IonicModule, QrCodeModule],
  exports: [ActionSuccessPage],
})
export class ActionSuccessPageModule {}
