import { NgModule } from '@angular/core';
import { PreloadAllModules, RouterModule, Routes } from '@angular/router';
const routes: Routes = [
  {
    path: 'embassy',
    loadChildren: () => import('./pages/embassy/embassy.module').then( m => m.EmbassyPageModule),
  },
  {
    path: 'loading',
    loadChildren: () => import('./pages/loading/loading.module').then( m => m.LoadingPageModule),
  },
  {
    path: 'product-key',
    loadChildren: () => import('./pages/product-key/product-key.module').then( m => m.ProductKeyPageModule),
  },
  {
    path: 'recover',
    loadChildren: () => import('./pages/recover/recover.module').then( m => m.RecoverPageModule),
  },
  {
    path: 'home',
    loadChildren: () => import('./pages/home/home.module').then( m => m.HomePageModule),
  },
  {
    path: 'success',
    loadChildren: () => import('./pages/success/success.module').then( m => m.SuccessPageModule),
  },
];

@NgModule({
  imports: [
    RouterModule.forRoot(routes, { preloadingStrategy: PreloadAllModules })
  ],
  exports: [RouterModule]
})
export class AppRoutingModule { }
