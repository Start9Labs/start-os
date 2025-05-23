import { Routes } from '@angular/router'

const MARKETPLACE_ROUTES: Routes = [
  {
    path: '',
    pathMatch: 'full',
    loadComponent: () => import('./marketplace.component'),
  },
]

export default MARKETPLACE_ROUTES
