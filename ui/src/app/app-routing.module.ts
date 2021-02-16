import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { AuthGuard } from './guards/auth.guard'
import { UnauthGuard } from './guards/unauth.guard'
import { MaintenanceGuard } from './guards/maintenance.guard'
import { UnmaintenanceGuard } from './guards/unmaintenance.guard'

const routes: Routes = [
  {
    redirectTo: 'services',
    pathMatch: 'full',
    path: '',
  },
  {
    path: 'auth',
    canActivate: [UnauthGuard],
    loadChildren: () => import('./pages/auth-routes/auth-routing.module').then(m => m.AuthRoutingModule),
  },
  {
    path: 'embassy',
    canActivate: [AuthGuard, MaintenanceGuard],
    canActivateChild: [AuthGuard, MaintenanceGuard],
    loadChildren: () => import('./pages/server-routes/server-routing.module').then(m => m.ServerRoutingModule),
  },
  {
    path: 'maintenance',
    canActivate: [AuthGuard, UnmaintenanceGuard],
    loadChildren: () => import('./pages/maintenance/maintenance.module').then(m => m.MaintenancePageModule),
  },
  {
    path: 'notifications',
    canActivate: [AuthGuard, MaintenanceGuard],
    loadChildren: () => import('./pages/notifications/notifications.module').then(m => m.NotificationsPageModule),
  },
  {
    path: 'services',
    canActivate: [AuthGuard, MaintenanceGuard],
    canActivateChild: [AuthGuard, MaintenanceGuard],
    loadChildren: () => import('./pages/apps-routes/apps-routing.module').then(m => m.AppsRoutingModule),
  },
]

@NgModule({
  imports: [
    RouterModule.forRoot(routes, {
      preloadingStrategy: PreloadAllModules,
      initialNavigation: 'disabled',
      useHash: true,
    }),
  ],
  exports: [RouterModule],
})
export class AppRoutingModule { }
