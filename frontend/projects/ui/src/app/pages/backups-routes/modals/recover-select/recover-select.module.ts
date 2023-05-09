import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { RecoverSelectPage } from './recover-select.page'
import { ToOptionsPipe } from './to-options.pipe'

@NgModule({
  declarations: [RecoverSelectPage, ToOptionsPipe],
  imports: [CommonModule, IonicModule, FormsModule],
  exports: [RecoverSelectPage],
})
export class RecoverSelectPageModule {}
