import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiFilterPipeModule } from '@taiga-ui/cdk'

import { AddWidgetComponent } from './add.component'

@NgModule({
  imports: [CommonModule, TuiFilterPipeModule],
  declarations: [AddWidgetComponent],
  exports: [AddWidgetComponent],
})
export class AddWidgetModule {}
