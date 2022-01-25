import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TextSpinnerComponent } from './text-spinner.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'

@NgModule({
  declarations: [
    TextSpinnerComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
  ],
  exports: [TextSpinnerComponent],
})
export class TextSpinnerComponentModule { }
