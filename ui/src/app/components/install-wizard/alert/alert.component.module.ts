import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { AlertComponent } from './alert.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    AlertComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [AlertComponent],
})
export class AlertComponentModule { }
