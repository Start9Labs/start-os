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
    path: ':pkgId/config',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: ':pkgId/config/:edit',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: ':pkgId/instructions',
    loadChildren: () => import('./app-instructions/app-instructions.module').then(m => m.AppInstructionsPageModule),
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
    path: ':pkgId/manifest',
    loadChildren: () => import('./app-manifest/app-manifest.module').then(m => m.AppManifestPageModule),
  },
  {
    path: ':pkgId/metrics',
    loadChildren: () => import('./app-metrics/app-metrics.module').then(m => m.AppMetricsPageModule),
  },
  {
    path: ':pkgId/properties',
    loadChildren: () => import('./app-properties/app-properties.module').then(m => m.AppPropertiesPageModule),
  },
  {
    path: ':pkgId/restore',
    loadChildren: () => import('./app-restore/app-restore.module').then(m => m.AppRestorePageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AppsRoutingModule { }