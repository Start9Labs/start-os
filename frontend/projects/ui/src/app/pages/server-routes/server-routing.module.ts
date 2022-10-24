import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./server-show/server-show.module').then(
        m => m.ServerShowPageModule,
      ),
  },
  {
    path: 'backup',
    loadChildren: () =>
      import('./server-backup/server-backup.module').then(
        m => m.ServerBackupPageModule,
      ),
  },
  {
    path: 'lan',
    loadChildren: () => import('./lan/lan.module').then(m => m.LANPageModule),
  },
  {
    path: 'logs',
    loadChildren: () =>
      import('./server-logs/server-logs.module').then(
        m => m.ServerLogsPageModule,
      ),
  },
  {
    path: 'kernel-logs',
    loadChildren: () =>
      import('./kernel-logs/kernel-logs.module').then(
        m => m.KernelLogsPageModule,
      ),
  },
  {
    path: 'marketplaces',
    loadChildren: () =>
      import('./marketplaces/marketplaces.module').then(
        m => m.MarketplacesPageModule,
      ),
  },
  {
    path: 'metrics',
    loadChildren: () =>
      import('./server-metrics/server-metrics.module').then(
        m => m.ServerMetricsPageModule,
      ),
  },
  {
    path: 'restore',
    loadChildren: () =>
      import('./restore/restore.component.module').then(
        m => m.RestorePageModule,
      ),
  },
  {
    path: 'sessions',
    loadChildren: () =>
      import('./sessions/sessions.module').then(m => m.SessionsPageModule),
  },
  {
    path: 'sideload',
    loadChildren: () =>
      import('./sideload/sideload.module').then(m => m.SideloadPageModule),
  },
  {
    path: 'specs',
    loadChildren: () =>
      import('./server-specs/server-specs.module').then(
        m => m.ServerSpecsPageModule,
      ),
  },
  {
    path: 'ssh',
    loadChildren: () =>
      import('./ssh-keys/ssh-keys.module').then(m => m.SSHKeysPageModule),
  },
  {
    path: 'wifi',
    loadChildren: () =>
      import('./wifi/wifi.module').then(m => m.WifiPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServerRoutingModule {}
