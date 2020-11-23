import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { StatusComponent } from './status.component'
import { IonicModule } from '@ionic/angular'

@NgModule({
  declarations: [
    StatusComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
  ],
  exports: [StatusComponent],
})
export class StatusComponentModule { }
