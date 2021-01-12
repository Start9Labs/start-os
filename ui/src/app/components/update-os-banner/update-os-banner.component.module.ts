import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { UpdateOsBannerComponent } from './update-os-banner.component'
import { IonicModule } from '@ionic/angular'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    UpdateOsBannerComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [UpdateOsBannerComponent],
})
export class UpdateOsBannerComponentModule { }
