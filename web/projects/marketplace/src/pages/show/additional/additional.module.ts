import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { AdditionalComponent } from './additional.component'
import { TuiButton, TuiLabel, TuiTitle } from '@taiga-ui/core'
import { MarketplaceAdditionalItemComponent } from './additional-item.component'
import { MarketplaceAdditionalLinkComponent } from './additional-link.component'

@NgModule({
  imports: [
    CommonModule,
    TuiButton,
    TuiLabel,
    MarketplaceAdditionalItemComponent,
    MarketplaceAdditionalLinkComponent,
    TuiTitle,
  ],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}
