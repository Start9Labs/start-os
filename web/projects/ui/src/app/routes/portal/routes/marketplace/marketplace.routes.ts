import { Routes } from '@angular/router'
import {
  AbstractMarketplaceService,
  MARKETPLACE_REGISTRY_ALERTS,
} from '@start9labs/marketplace'

import { MarketplaceService } from 'src/app/services/marketplace.service'

import { MarketplaceAlertsService } from './services/alerts.service'

const MARKETPLACE_ROUTES: Routes = [
  {
    path: '',
    pathMatch: 'full',
    providers: [
      { provide: AbstractMarketplaceService, useExisting: MarketplaceService },
      {
        provide: MARKETPLACE_REGISTRY_ALERTS,
        useExisting: MarketplaceAlertsService,
      },
    ],
    loadComponent: () => import('./marketplace.component'),
  },
]

export default MARKETPLACE_ROUTES
