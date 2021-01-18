import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { AuthGuard } from './guards/auth.guard'
import { UnauthGuard } from './guards/unauth.guard'

const routes: Routes = [
  {
    redirectTo: 'services',
    pathMatch: 'full',
    path: '',
  },
  {
    path: 'authenticate',
    canActivate: [UnauthGuard],
    pathMatch: 'full',
    loadChildren: () => import('./pages/authenticate/authenticate.module').then( m => m.AuthenticatePageModule),
  },
  {
    path: 'embassy',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () => import('./pages/server-routes/server-routing.module').then(m => m.ServerRoutingModule),
  },
  {
    path: 'notifications',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () => import('./pages/notifications/notifications.module').then(m => m.NotificationsPageModule),
  },
  {
    path: 'services',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () => import('./pages/apps-routes/apps-routing.module').then(m => m.AppsRoutingModule),
  },
  // {
  //   path: 'drives',
  //   canActivate: [AuthGuard],
  //   loadChildren: () => import('./pages/server-routes/external-drives/external-drives.module').then( m => m.ExternalDrivesPageModule),
  // },
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
