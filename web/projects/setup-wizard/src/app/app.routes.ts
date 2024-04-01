import { Routes } from '@angular/router'

export const ROUTES: Routes = [
  { path: '', redirectTo: '/home', pathMatch: 'full' },
  {
    path: 'home',
    loadComponent: () => import('src/app/pages/home.page'),
  },
  {
    path: 'attach',
    loadComponent: () => import('src/app/pages/attach.page'),
  },
  {
    path: 'recover',
    loadComponent: () => import('src/app/pages/recover.page'),
  },
  {
    path: 'transfer',
    loadComponent: () => import('src/app/pages/transfer.page'),
  },
  {
    path: 'storage',
    loadComponent: () => import('src/app/pages/storage.page'),
  },
  {
    path: 'loading',
    loadComponent: () => import('src/app/pages/loading.page'),
  },
  {
    path: 'success',
    loadComponent: () => import('src/app/pages/success.page'),
  },
]
