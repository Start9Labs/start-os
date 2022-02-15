import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TextSpinnerComponent } from './text-spinner.component'

@NgModule({
  declarations: [TextSpinnerComponent],
  imports: [CommonModule, IonicModule],
  exports: [TextSpinnerComponent],
})
export class TextSpinnerComponentModule {}
