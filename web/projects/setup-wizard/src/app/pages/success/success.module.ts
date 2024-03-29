import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { ResponsiveColModule } from '@start9labs/shared'

import { SuccessPage } from './success.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { SuccessPageRoutingModule } from './success-routing.module'
import { DownloadDocComponent } from './download-doc/download-doc.component'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    PasswordPageModule,
    SuccessPageRoutingModule,
    ResponsiveColModule,
  ],
  declarations: [SuccessPage, DownloadDocComponent],
  exports: [SuccessPage],
})
export class SuccessPageModule {}
