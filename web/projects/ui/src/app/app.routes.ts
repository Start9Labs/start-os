import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { AuthGuard } from 'src/app/guards/auth.guard'
import { UnauthGuard } from 'src/app/guards/unauth.guard'
import { stateNot } from 'src/app/services/state.service'

export const ROUTES: Routes = [
  {
    path: 'diagnostic',
    canActivate: [stateNot(['initializing', 'running'])],
    loadChildren: () => import('./routes/diagnostic/diagnostic.routes'),
  },
  {
    path: 'initializing',
    canActivate: [stateNot(['error', 'running'])],
    loadComponent: () => import('./routes/initializing/initializing.page'),
  },
  {
    path: 'login',
    canActivate: [UnauthGuard, stateNot(['error', 'initializing'])],
    loadComponent: () => import('./routes/login/login.page'),
  },
  {
    path: '',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    loadChildren: () => import('./routes/portal/portal.routes'),
  },
  {
    path: '**',
    redirectTo: '',
    pathMatch: 'full',
  },
]
