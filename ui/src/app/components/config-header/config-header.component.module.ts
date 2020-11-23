import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ConfigHeaderComponent } from './config-header.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    ConfigHeaderComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [ConfigHeaderComponent],
})
export class ConfigHeaderComponentModule { }
