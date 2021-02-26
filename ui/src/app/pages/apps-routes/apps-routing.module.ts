import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'installed',
    pathMatch: 'full',
  },
  {
    path: 'marketplace',
    loadChildren: () => import('./app-available-list/app-available-list.module').then(m => m.AppAvailableListPageModule),
  },
  {
    path: 'installed',
    loadChildren: () => import('./app-installed-list/app-installed-list.module').then(m => m.AppInstalledListPageModule),
  },
  {
    path: 'marketplace/:appId',
    loadChildren: () => import('./app-available-show/app-available-show.module').then(m => m.AppAvailableShowPageModule),
  },
  {
    path: 'installed/:appId',
    loadChildren: () => import('./app-installed-show/app-installed-show.module').then(m => m.AppInstalledShowPageModule),
  },
  {
    path: 'installed/:appId/instructions',
    loadChildren: () => import('./app-instructions/app-instructions.module').then(m => m.AppInstructionsPageModule),
  },
  {
    path: 'installed/:appId/config',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: 'installed/:appId/config/:edit',
    loadChildren: () => import('./app-config/app-config.module').then(m => m.AppConfigPageModule),
  },
  {
    path: 'installed/:appId/logs',
    loadChildren: () => import('./app-logs/app-logs.module').then(m => m.AppLogsPageModule),
  },
  {
    path: 'installed/:appId/metrics',
    loadChildren: () => import('./app-metrics/app-metrics.module').then(m => m.AppMetricsPageModule),
  },
  {
    path: 'installed/:appId/actions',
    loadChildren: () => import('./app-actions/app-actions.module').then(m => m.AppActionsPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AppsRoutingModule { }