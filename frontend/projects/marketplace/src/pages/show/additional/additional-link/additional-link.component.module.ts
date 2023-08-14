import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { AdditionalLinkComponent } from './additional-link.component'
import { TuiLabelModule } from '@taiga-ui/core'
import { TuiLineClampModule } from '@taiga-ui/kit'

@NgModule({
  imports: [CommonModule, TuiLabelModule, TuiLineClampModule],
  declarations: [AdditionalLinkComponent],
  exports: [AdditionalLinkComponent],
})
export class AdditionalLinkModule {}
