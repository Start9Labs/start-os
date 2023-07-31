import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'list',
    pathMatch: 'full',
  },
  {
    path: 'list',
    loadChildren: () =>
      import('./app-list/app-list.module').then(m => m.AppListPageModule),
  },
  {
    path: ':pkgId',
    loadChildren: () =>
      import('./app-show/app-show.module').then(m => m.AppShowPageModule),
  },
  {
    path: ':pkgId/actions',
    loadChildren: () =>
      import('./app-actions/app-actions.module').then(
        m => m.AppActionsPageModule,
      ),
  },
  {
    path: ':pkgId/logs',
    loadChildren: () =>
      import('./app-logs/app-logs.module').then(m => m.AppLogsPageModule),
  },
  {
    path: ':pkgId/credentials',
    loadChildren: () =>
      import('./app-credentials/app-credentials.module').then(
        m => m.AppCredentialsPageModule,
      ),
  },
  {
    path: ':pkgId/interfaces/:interfaceId',
    loadChildren: () =>
      import('./app-interface/app-interface.module').then(
        m => m.AppInterfacePageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServicesModule {}
