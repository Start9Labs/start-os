import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiLetModule } from '@taiga-ui/cdk'
import { TuiProgressModule } from '@taiga-ui/kit'

import { LogsWindowComponent } from './logs-window.component'
import { InitializingComponent } from './initializing.component'

@NgModule({
  imports: [CommonModule, TuiLetModule, LogsWindowComponent, TuiProgressModule],
  declarations: [InitializingComponent],
  exports: [InitializingComponent],
})
export class InitializingModule {}
