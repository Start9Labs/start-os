import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule } from '@start9labs/shared'
import { MarketplacePipesModule } from '@start9labs/marketplace'

import { MarketplaceStatusComponent } from './marketplace-status.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    EmverPipesModule,
    MarketplacePipesModule,
  ],
  declarations: [MarketplaceStatusComponent],
  exports: [MarketplaceStatusComponent],
})
export class MarketplaceStatusModule {}
