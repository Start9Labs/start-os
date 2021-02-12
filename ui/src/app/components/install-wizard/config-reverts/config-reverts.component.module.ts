import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ConfigRevertsComponent } from './config-reverts.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    ConfigRevertsComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [ConfigRevertsComponent],
})
export class ConfigRevertsComponentModule { }
