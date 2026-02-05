import { Routes } from '@angular/router'
import { Outlet } from 'src/app/routes/home/components/outlet'

export default [
  {
    path: '',
    component: Outlet,
    children: [
      {
        path: 'wan',
        loadChildren: () => import('./routes/wan'),
      },
      {
        path: 'published-ports',
        loadComponent: () => import('./routes/published-ports'),
      },
      {
        path: 'ethernet',
        loadComponent: () => import('./routes/ethernet'),
      },
      {
        path: 'wifi',
        loadChildren: () => import('./routes/wifi'),
      },
      {
        path: 'outbound',
        loadChildren: () => import('./routes/outbound'),
      },
      {
        path: 'inbound',
        loadChildren: () => import('./routes/inbound'),
      },
      {
        path: 'lan',
        loadChildren: () => import('./routes/lan'),
      },
      {
        path: 'devices',
        loadChildren: () => import('./routes/devices'),
      },
      {
        path: 'settings',
        loadChildren: () => import('./routes/settings'),
      },
      { path: '**', redirectTo: 'wan' },
    ],
  },
] satisfies Routes
