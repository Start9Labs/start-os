import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'
import { NavGuard, RecoveryNavGuard } from './guards/nav-guard'

const routes: Routes = [
  { path: '', redirectTo: '/product-key', pathMatch: 'full' },
  {
    path: 'init',
    loadChildren: () => import('./pages/init/init.module').then( m => m.InitPageModule),
    canActivate: [NavGuard],
  },
  {
    path: 'product-key',
    loadChildren: () => import('./pages/product-key/product-key.module').then( m => m.ProductKeyPageModule),
  },
  {
    path: 'home',
    loadChildren: () => import('./pages/home/home.module').then( m => m.HomePageModule),
    canActivate: [NavGuard],
  },
  {
    path: 'recover',
    loadChildren: () => import('./pages/recover/recover.module').then( m => m.RecoverPageModule),
    canActivate: [RecoveryNavGuard],
  },
  {
    path: 'embassy',
    loadChildren: () => import('./pages/embassy/embassy.module').then( m => m.EmbassyPageModule),
    canActivate: [NavGuard],
  },
  {
    path: 'loading',
    loadChildren: () => import('./pages/loading/loading.module').then( m => m.LoadingPageModule),
    canActivate: [NavGuard],
  },
]

@NgModule({
  imports: [
    RouterModule.forRoot(routes, {
      scrollPositionRestoration: 'enabled',
      preloadingStrategy: PreloadAllModules,
      useHash: true,
      initialNavigation: 'disabled',
    }),
  ],
  exports: [RouterModule],
})
export class AppRoutingModule { }
