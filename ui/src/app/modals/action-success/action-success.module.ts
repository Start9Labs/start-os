import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ActionSuccessPage } from './action-success.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'
import { QRComponentModule } from 'src/app/components/qr/qr.component.module'
import { QrCodeModule } from 'ng-qrcode'

@NgModule({
  declarations: [ActionSuccessPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    SharingModule,
    QrCodeModule,
  ],
  exports: [ActionSuccessPage],
})
export class ActionSuccessPageModule { }
