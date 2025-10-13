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
        path: 'outbound',
        loadChildren: () => import('./routes/outbound'),
      },
      {
        path: 'settings',
        loadChildren: () => import('./routes/settings'),
      },
      { path: '**', redirectTo: 'wan' },
    ],
  },
] satisfies Routes
