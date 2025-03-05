import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { stateNot } from 'src/app/services/state.service'
import { AuthGuard } from './guards/auth.guard'
import { UnauthGuard } from './guards/unauth.guard'

const routes: Routes = [
  {
    path: 'diagnostic',
    canActivate: [stateNot(['initializing', 'running'])],
    loadChildren: () => import('./routes/diagnostic/diagnostic.module'),
  },
  {
    path: 'initializing',
    canActivate: [stateNot(['error', 'running'])],
    loadComponent: () => import('./routes/initializing/initializing.page'),
  },
  {
    path: 'login',
    canActivate: [UnauthGuard, stateNot(['error', 'initializing'])],
    loadChildren: () =>
      import('./routes/login/login.module').then(m => m.LoginPageModule),
  },
  {
    path: 'portal',
    canActivate: [AuthGuard, stateNot(['error', 'initializing'])],
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
      paramsInheritanceStrategy: 'always',
      preloadingStrategy: PreloadAllModules,
      initialNavigation: 'disabled',
      bindToComponentInputs: true,
    }),
  ],
  exports: [RouterModule],
})
export class RoutingModule {}
