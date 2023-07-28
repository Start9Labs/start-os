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
    path: 'addresses',
    loadChildren: () =>
      import('./os-addresses/os-addresses.module').then(
        m => m.OSAddressesPageModule,
      ),
  },
  {
    path: 'port-forwards',
    loadChildren: () =>
      import('./port-forwards/port-forwards.module').then(
        m => m.PortForwardsPageModule,
      ),
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
