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
    path: 'wifi',
    loadChildren: () => import('./wifi/wifi.module').then(m => m.WifiPageModule),
  },
  {
    path: 'lan',
    loadChildren: () => import('./lan/lan.module').then(m => m.LANPageModule),
  },
  {
    path: 'security',
    loadChildren: () => import('./security-routes/security-routing.module').then( m => m.SecurityRoutingModule),
  },
  {
    path: 'preferences',
    loadChildren: () => import('./preferences/preferences.module').then( m => m.PreferencesPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServerRoutingModule { }