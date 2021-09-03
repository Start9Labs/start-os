import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { PwaBackComponent } from './pwa-back.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'

@NgModule({
  declarations: [
    PwaBackComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
  ],
  exports: [PwaBackComponent],
})
export class PwaBackComponentModule { }
