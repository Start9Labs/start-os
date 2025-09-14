import { Routes } from '@angular/router'
import { Outlet } from 'src/app/routes/home/components/outlet'

export default [
  {
    path: '',
    component: Outlet,
    children: [
      {
        path: 'wan',
        loadComponent: () => import('./routes/wan'),
      },
      {
        path: 'outbound',
        loadComponent: () => import('./routes/outbound'),
      },
      { path: '**', redirectTo: 'wan' },
    ],
  },
] satisfies Routes
