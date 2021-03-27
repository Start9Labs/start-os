import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ConfigRevertsComponent } from './config-reverts.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [
    ConfigRevertsComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    FormsModule,
  ],
  exports: [ConfigRevertsComponent],
})
export class ConfigRevertsComponentModule { }
