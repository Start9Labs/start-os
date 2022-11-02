import { NgModule } from '@angular/core'
import { PreloadAllModules, RouterModule, Routes } from '@angular/router'

const routes: Routes = [
  { path: '', redirectTo: '/home', pathMatch: 'full' },
  {
    path: 'home',
    loadChildren: () =>
      import('./pages/home/home.module').then(m => m.HomePageModule),
  },
  {
    path: 'recover',
    loadChildren: () =>
      import('./pages/recover/recover.module').then(m => m.RecoverPageModule),
  },
  {
    path: 'transfer',
    loadChildren: () =>
      import('./pages/transfer/transfer.module').then(
        m => m.TransferPageModule,
      ),
  },
  {
    path: 'embassy',
    loadChildren: () =>
      import('./pages/embassy/embassy.module').then(m => m.EmbassyPageModule),
  },
  {
    path: 'loading',
    loadChildren: () =>
      import('./pages/loading/loading.module').then(m => m.LoadingPageModule),
  },
  {
    path: 'success',
    loadChildren: () =>
      import('./pages/success/success.module').then(m => m.SuccessPageModule),
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
export class AppRoutingModule {}
