import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { UpdateOsBannerComponent } from './update-os-banner.component'
import { IonicModule } from '@ionic/angular'

@NgModule({
  declarations: [
    UpdateOsBannerComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
  ],
  exports: [UpdateOsBannerComponent],
})
export class UpdateOsBannerComponentModule { }
