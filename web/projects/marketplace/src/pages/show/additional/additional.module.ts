import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { AdditionalComponent } from './additional.component'
import { TuiButtonModule, TuiLabelModule } from '@taiga-ui/core'
import { MarketplaceAdditionalItemComponent } from './additional-item.component'
import { MarketplaceAdditionalLinkComponent } from './additional-link.component'

@NgModule({
  imports: [
    CommonModule,
    TuiButtonModule,
    TuiLabelModule,
    MarketplaceAdditionalItemComponent,
    MarketplaceAdditionalLinkComponent,
  ],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}
