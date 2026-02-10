import { Routes } from '@angular/router'

export const ROUTES: Routes = [
  {
    path: '',
    loadComponent: () => import('./routes/outlet.component'),
  },
  {
    path: 'kernel',
    loadComponent: () => import('./routes/kernel.component'),
  },
  {
    path: 'os',
    loadComponent: () => import('./routes/os.component'),
  },
]

export default ROUTES
