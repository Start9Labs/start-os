import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ActionSuccessPage } from './action-success.page'
import { QrCodeModule } from 'ng-qrcode'

@NgModule({
  declarations: [ActionSuccessPage],
  imports: [
    CommonModule,
    IonicModule,
    QrCodeModule,
  ],
  exports: [ActionSuccessPage],
})
export class ActionSuccessPageModule { }
