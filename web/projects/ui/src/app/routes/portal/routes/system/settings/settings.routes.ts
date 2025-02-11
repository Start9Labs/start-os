import { SettingsComponent } from './settings.component'

export default [
  {
    path: '',
    component: SettingsComponent,
    children: [
      {
        path: 'acme',
        loadComponent: () =>
          import('./routes/acme/acme.component').then(
            m => m.SettingsACMEComponent,
          ),
      },
      {
        path: 'email',
        loadComponent: () =>
          import('./routes/email/email.component').then(
            m => m.SettingsEmailComponent,
          ),
      },
      {
        path: 'domains',
        loadComponent: () =>
          import('./routes/domains/domains.component').then(
            m => m.SettingsDomainsComponent,
          ),
      },
      {
        path: 'proxies',
        loadComponent: () =>
          import('./routes/proxies/proxies.component').then(
            m => m.SettingsProxiesComponent,
          ),
      },
      {
        path: 'router',
        loadComponent: () =>
          import('./routes/router/router.component').then(
            m => m.SettingsRouterComponent,
          ),
      },
      {
        path: 'wifi',
        loadComponent: () =>
          import('./routes/wifi/wifi.component').then(
            m => m.SettingsWifiComponent,
          ),
      },
      {
        path: 'ui',
        loadComponent: () =>
          import('./routes/interfaces/ui.component').then(
            m => m.StartOsUiComponent,
          ),
      },
      {
        path: 'ssh',
        loadComponent: () =>
          import('./routes/ssh/ssh.component').then(
            m => m.SettingsSSHComponent,
          ),
      },
      {
        path: 'sessions',
        loadComponent: () =>
          import('./routes/sessions/sessions.component').then(
            m => m.SettingsSessionsComponent,
          ),
      },
    ],
  },
]
