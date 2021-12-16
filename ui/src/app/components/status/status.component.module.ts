import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { StatusComponent } from './status.component'
import { IonicModule } from '@ionic/angular'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    StatusComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [StatusComponent],
})
export class StatusComponentModule { }
