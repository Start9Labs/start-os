import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { AuthGuard } from './guards/auth.guard'
import { UnauthGuard } from './guards/unauth.guard'

const routes: Routes = [
  {
    path: 'diagnostic',
    loadChildren: () =>
      import('./apps/diagnostic/diagnostic.module').then(
        m => m.DiagnosticModule,
      ),
  },
  {
    path: 'loading',
    loadChildren: () =>
      import('./apps/loading/loading.module').then(m => m.LoadingPageModule),
  },
  {
    path: 'login',
    canActivate: [UnauthGuard],
    loadChildren: () =>
      import('./apps/login/login.module').then(m => m.LoginPageModule),
  },
  {
    path: 'portal',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () =>
      import('./apps/portal/portal.module').then(m => m.PortalModule),
  },
  {
    path: '',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () => import('./apps/ui/ui.module').then(m => m.UiModule),
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
export class RoutingModule {}
