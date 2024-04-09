import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { AuthGuard } from './guards/auth.guard'
import { UnauthGuard } from './guards/unauth.guard'

const routes: Routes = [
  {
    path: 'diagnostic',
    loadChildren: () =>
      import('./routes/diagnostic/diagnostic.module').then(
        m => m.DiagnosticModule,
      ),
  },
  {
    path: 'loading',
    loadComponent: () => import('./routes/loading/loading.page'),
  },
  {
    path: 'login',
    canActivate: [UnauthGuard],
    loadChildren: () =>
      import('./routes/login/login.module').then(m => m.LoginPageModule),
  },
  {
    path: 'portal',
    canActivate: [AuthGuard],
    canActivateChild: [AuthGuard],
    loadChildren: () => import('./routes/portal/portal.routes'),
  },
  {
    path: '',
    redirectTo: 'portal',
    pathMatch: 'full',
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
