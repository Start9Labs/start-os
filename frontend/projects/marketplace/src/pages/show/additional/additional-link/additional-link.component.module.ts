import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'

import { AdditionalLinkComponent } from './additional-link.component'
import { TuiLabelModule } from '@taiga-ui/core'

@NgModule({
  imports: [CommonModule, TuiLabelModule],
  declarations: [AdditionalLinkComponent],
  exports: [AdditionalLinkComponent],
})
export class AdditionalLinkModule {}
