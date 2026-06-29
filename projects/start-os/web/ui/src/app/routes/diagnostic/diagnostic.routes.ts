import { Routes } from '@angular/router'

export default [
  {
    path: '',
    loadComponent: () => import('./home/home.page'),
  },
  {
    path: 'logs',
    loadComponent: () => import('./logs.component'),
  },
] satisfies Routes
