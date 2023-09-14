import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./server-show/server-show.module').then(
        m => m.ServerShowPageModule,
      ),
  },
  {
    path: 'interfaces/ui',
    loadChildren: () =>
      import('./ui-details/ui-details.module').then(m => m.UIDetailsPageModule),
  },
  {
    path: 'router-config',
    loadChildren: () =>
      import('./router/router.module').then(m => m.RouterPageModule),
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
    path: 'tor-logs',
    loadChildren: () =>
      import('./tor-logs/tor-logs.module').then(m => m.TorLogsPageModule),
  },
  {
    path: 'metrics',
    loadChildren: () =>
      import('./server-metrics/server-metrics.module').then(
        m => m.ServerMetricsPageModule,
      ),
  },
  {
    path: 'sessions',
    loadChildren: () =>
      import('./sessions/sessions.module').then(m => m.SessionsPageModule),
  },
  {
    path: 'specs',
    loadChildren: () =>
      import('./server-specs/server-specs.module').then(
        m => m.ServerSpecsPageModule,
      ),
  },
  {
    path: 'domains',
    loadChildren: () =>
      import('./domains/domains.module').then(m => m.DomainsPageModule),
  },
  {
    path: 'proxies',
    loadChildren: () =>
      import('./proxies/proxies.module').then(m => m.ProxiesPageModule),
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
  {
    path: 'experimental-features',
    loadChildren: () =>
      import('./experimental-features/experimental-features.module').then(
        m => m.ExperimentalFeaturesPageModule,
      ),
  },
  {
    path: 'email',
    loadChildren: () =>
      import('./email/email.module').then(m => m.EmailPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SystemModule {}
