import { Routes } from '@angular/router'

const MARKETPLACE_ROUTES: Routes = [
  {
    path: '',
    pathMatch: 'full',
    loadComponent: () =>
      import('./marketplace.component').then(m => m.MarketplaceComponent),
  },
]

export default MARKETPLACE_ROUTES
