import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { stateNot } from 'src/app/services/state.service'
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
    path: 'diagnostic',
    canActivate: [stateNot(['initializing', 'running'])],
    loadChildren: () =>
      import('./pages/diagnostic-routes/diagnostic-routing.module').then(
        m => m.DiagnosticModule,
      ),
  },
  {
    path: 'initializing',
    canActivate: [stateNot(['error', 'running'])],
    loadChildren: () =>
      import('./pages/init/init.module').then(m => m.InitPageModule),
  },
  {
    path: 'home',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    loadChildren: () =>
      import('./pages/home/home.module').then(m => m.HomePageModule),
  },
  {
    path: 'system',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/server-routes/server-routing.module').then(
        m => m.ServerRoutingModule,
      ),
  },
  {
    path: 'updates',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/updates/updates.module').then(m => m.UpdatesPageModule),
  },
  {
    path: 'marketplace',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/marketplace-routes/marketplace-routing.module').then(
        m => m.MarketplaceRoutingModule,
      ),
  },
  {
    path: 'notifications',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    loadChildren: () =>
      import('./pages/notifications/notifications.module').then(
        m => m.NotificationsPageModule,
      ),
  },
  {
    path: 'services',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./pages/apps-routes/apps-routing.module').then(
        m => m.AppsRoutingModule,
      ),
  },
]

@NgModule({
  imports: [
    RouterModule.forRoot(routes, {
      scrollPositionRestoration: 'enabled',
      preloadingStrategy: PreloadAllModules,
      initialNavigation: 'disabled',
    }),
  ],
  exports: [RouterModule],
})
export class AppRoutingModule {}
