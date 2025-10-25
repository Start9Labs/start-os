import { inject } from '@angular/core'
import { Routes } from '@angular/router'
import { AuthService } from 'src/app/services/auth.service'

export const routes: Routes = [
  {
    path: '',
    loadChildren: () => import('./routes/home'),
    canMatch: [() => inject(AuthService).authenticated()],
  },
  {
    path: '',
    loadComponent: () => import('./routes/login'),
    canMatch: [() => !inject(AuthService).authenticated()],
  },
  { path: '**', redirectTo: '' },
]
