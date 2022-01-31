import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { CompleteComponent } from './complete.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'

@NgModule({
  declarations: [
    CompleteComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
  ],
  exports: [CompleteComponent],
})
export class CompleteComponentModule { }
