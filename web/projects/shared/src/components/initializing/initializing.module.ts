import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { TuiLetModule } from '@taiga-ui/cdk'

import { LogsWindowComponent } from './logs-window.component'
import { InitializingComponent } from './initializing.component'

@NgModule({
  imports: [CommonModule, IonicModule, TuiLetModule, LogsWindowComponent],
  declarations: [InitializingComponent],
  exports: [InitializingComponent],
})
export class InitializingModule {}
