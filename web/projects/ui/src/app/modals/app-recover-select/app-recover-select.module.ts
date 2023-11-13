import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'

import { AppRecoverSelectPage } from './app-recover-select.page'
import { ToOptionsPipe } from './to-options.pipe'

@NgModule({
  declarations: [AppRecoverSelectPage, ToOptionsPipe],
  imports: [CommonModule, IonicModule, FormsModule],
  exports: [AppRecoverSelectPage],
})
export class AppRecoverSelectPageModule {}
