import { Routes } from '@angular/router'

export default [
  {
    path: '',
    loadComponent: () => import('./routes/table'),
  },
  {
    path: ':id',
    loadComponent: () => import('./routes/device'),
  },
] satisfies Routes
