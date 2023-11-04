import { NgModule } from '@angular/core'
import { MarketplaceMenuComponent } from './marketplace-menu.component'
import { MenuModule } from '@start9labs/marketplace'
import { TuiButtonModule } from '@taiga-ui/core'

@NgModule({
  imports: [MenuModule, TuiButtonModule],
  exports: [MarketplaceMenuComponent],
  declarations: [MarketplaceMenuComponent],
})
export class MarketplaceMenuModule {}
