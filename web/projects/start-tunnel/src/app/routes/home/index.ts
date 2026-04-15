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
        title: 'Subnets',
      },
      {
        path: 'devices',
        loadComponent: () => import('./routes/devices'),
        title: 'Devices',
      },
      {
        path: 'port-forwards',
        loadComponent: () => import('./routes/port-forwards'),
        title: 'Port forwards',
      },
      {
        path: 'settings',
        loadComponent: () => import('./routes/settings'),
        title: 'Settings',
      },
      { path: '**', redirectTo: 'subnets' },
    ],
  },
] satisfies Routes
