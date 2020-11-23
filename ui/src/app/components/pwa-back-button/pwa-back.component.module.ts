import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { PwaBackComponent } from './pwa-back.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    PwaBackComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [PwaBackComponent],
})
export class PwaBackComponentModule { }
