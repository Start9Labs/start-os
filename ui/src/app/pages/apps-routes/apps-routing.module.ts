import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'installed',
    pathMatch: 'full',
  },
  {
    path: 'installed',
    loadChildren: () => import('./app-installed-list/app-installed-list.module').then(m => m.AppInstalledListPageModule),
  },
  {
    path: 'installed/:pkgId',
    loadChildren: () => import('./app-installed-show/app-installed-show.module').then(m => m.AppInstalledShowPageModule),
  },
  {
    path: 'installed/:pkgId/actions',
    loadChildren: () => import('./app-actions/app-actions.module').then(m => m.AppActionsPageModule),
  },
  {
    path: 'installed/:pkgId/config',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: 'installed/:pkgId/config/:edit',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: 'installed/:pkgId/instructions',
    loadChildren: () => import('./app-instructions/app-instructions.module').then(m => m.AppInstructionsPageModule),
  },
  {
    path: 'installed/:pkgId/interfaces',
    loadChildren: () => import('./app-interfaces/app-interfaces.module').then(m => m.AppInterfacesPageModule),
  },
  {
    path: 'installed/:pkgId/logs',
    loadChildren: () => import('./app-logs/app-logs.module').then(m => m.AppLogsPageModule),
  },
  {
    path: 'installed/:pkgId/manifest',
    loadChildren: () => import('./app-manifest/app-manifest.module').then(m => m.AppManifestPageModule),
  },
  {
    path: 'installed/:pkgId/metrics',
    loadChildren: () => import('./app-metrics/app-metrics.module').then(m => m.AppMetricsPageModule),
  },
  {
    path: 'installed/:pkgId/properties',
    loadChildren: () => import('./app-properties/app-properties.module').then(m => m.AppPropertiesPageModule),
  },
  {
    path: 'installed/:pkgId/restore',
    loadChildren: () => import('./app-restore/app-restore.module').then(m => m.AppRestorePageModule),
  },
  {
    path: 'marketplace',
    loadChildren: () => import('./app-available-list/app-available-list.module').then(m => m.AppAvailableListPageModule),
  },
  {
    path: 'marketplace/:pkgId',
    loadChildren: () => import('./app-available-show/app-available-show.module').then(m => m.AppAvailableShowPageModule),
  },
  {
    path: 'marketplace/:pkgId/notes',
    loadChildren: () => import('./release-notes/release-notes.module').then(m => m.ReleaseNotesModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AppsRoutingModule { }