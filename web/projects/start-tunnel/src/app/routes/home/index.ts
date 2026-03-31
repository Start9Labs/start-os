import { Routes } from '@angular/router'
import { Outlet } from 'src/app/routes/home/components/outlet'

export default [
  {
    path: '',
    component: Outlet,
    children: [
      {
        path: 'subnets',
        loadComponent: () => import('./routes/subnets'),
      },
      {
        path: 'devices',
        loadComponent: () => import('./routes/devices'),
      },
      {
        path: 'port-forwards',
        loadComponent: () => import('./routes/port-forwards'),
      },
      {
        path: 'settings',
        loadComponent: () => import('./routes/settings'),
      },
      { path: '**', redirectTo: 'subnets' },
    ],
  },
] satisfies Routes
