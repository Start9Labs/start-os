import { NgModule } from '@angular/core'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@NgModule({
  providers: [
    {
      provide: AbstractMarketplaceService,
      useClass: MarketplaceService,
    },
  ],
})
export class MarketplaceModule {}
