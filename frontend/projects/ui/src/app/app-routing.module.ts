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
    path: 'login',
    canActivate: [UnauthGuard],
    loadChildren: () =>
      import('./pages/login/login.module').then(m => m.LoginPageModule),
  },
  {
    path: 'embassy',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/server-routes/server-routing.module').then(
        m => m.ServerRoutingModule,
      ),
  },
  {
    path: 'marketplace',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/marketplace-routes/marketplace-routing.module').then(
        m => m.MarketplaceRoutingModule,
      ),
  },
  {
    path: 'notifications',
    canActivate: [AuthGuard],
    loadChildren: () =>
      import('./pages/notifications/notifications.module').then(
        m => m.NotificationsPageModule,
      ),
  },
  {
    path: 'services',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/apps-routes/apps-routing.module').then(
        m => m.AppsRoutingModule,
      ),
  },
  {
    path: 'developer',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/developer-routes/developer-routing.module').then(
        m => m.DeveloperRoutingModule,
      ),
  },
]

@NgModule({
  imports: [
    RouterModule.forRoot(routes, {
      scrollPositionRestoration: 'enabled',
      preloadingStrategy: PreloadAllModules,
      initialNavigation: 'disabled',
      useHash: true,
    }),
  ],
  exports: [RouterModule],
})
export class AppRoutingModule {}
