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
    loadChildren: () => import('./app-list/app-list.module').then(m => m.AppListPageModule),
  },
  {
    path: ':pkgId',
    loadChildren: () => import('./app-show/app-show.module').then(m => m.AppShowPageModule),
  },
  {
    path: ':pkgId/actions',
    loadChildren: () => import('./app-actions/app-actions.module').then(m => m.AppActionsPageModule),
  },
  {
    path: ':pkgId/interfaces',
    loadChildren: () => import('./app-interfaces/app-interfaces.module').then(m => m.AppInterfacesPageModule),
  },
  {
    path: ':pkgId/logs',
    loadChildren: () => import('./app-logs/app-logs.module').then(m => m.AppLogsPageModule),
  },
  {
    path: ':pkgId/metrics',
    loadChildren: () => import('./app-metrics/app-metrics.module').then(m => m.AppMetricsPageModule),
  },
  {
    path: ':pkgId/properties',
    loadChildren: () => import('./app-properties/app-properties.module').then(m => m.AppPropertiesPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AppsRoutingModule { }