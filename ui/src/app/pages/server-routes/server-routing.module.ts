import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () => import('./server-show/server-show.module').then(m => m.ServerShowPageModule),
  },
  {
    path: 'backup',
    loadChildren: () => import('./server-backup/server-backup.module').then(m => m.ServerBackupPageModule),
  },
  {
    path: 'specs',
    loadChildren: () => import('./server-specs/server-specs.module').then(m => m.ServerSpecsPageModule),
  },
  {
    path: 'metrics',
    loadChildren: () => import('./server-metrics/server-metrics.module').then(m => m.ServerMetricsPageModule),
  },
  {
    path: 'logs',
    loadChildren: () => import('./server-logs/server-logs.module').then(m => m.ServerLogsPageModule),
  },
  {
    path: 'settings',
    loadChildren: () => import('./general-settings/general-settings.module').then(m => m.GeneralSettingsPageModule),
  },
  {
    path: 'wifi',
    loadChildren: () => import('./wifi/wifi.module').then(m => m.WifiListPageModule),
  },
  {
    path: 'lan',
    loadChildren: () => import('./lan/lan.module').then(m => m.LANPageModule),
  },
  {
    path: 'developer',
    loadChildren: () => import('./developer-routes/developer-routing.module').then( m => m.DeveloperRoutingModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServerRoutingModule { }