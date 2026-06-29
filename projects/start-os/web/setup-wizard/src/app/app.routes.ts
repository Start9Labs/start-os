import { Routes } from '@angular/router'

export const ROUTES: Routes = [
  // Entry point - app.component handles initial routing based on setup.status
  { path: '', redirectTo: '/language', pathMatch: 'full' },

  // Install flow
  {
    path: 'language',
    loadComponent: () => import('./pages/language.page'),
  },
  {
    path: 'keyboard',
    loadComponent: () => import('./pages/keyboard.page'),
  },
  {
    path: 'drives',
    loadComponent: () => import('./pages/drives.page'),
  },

  // Setup flow (after install or for pre-installed devices)
  {
    path: 'home',
    loadComponent: () => import('./pages/home.page'),
  },
  {
    path: 'restore',
    loadComponent: () => import('./pages/restore.page'),
  },
  {
    path: 'transfer',
    loadComponent: () => import('./pages/transfer.page'),
  },
  {
    path: 'password',
    loadComponent: () => import('./pages/password.page'),
  },

  // Shared
  {
    path: 'loading',
    loadComponent: () => import('./pages/loading.page'),
  },
  {
    path: 'success',
    loadComponent: () => import('./pages/success.page'),
  },
]
