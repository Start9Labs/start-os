import { Routes } from '@angular/router'

import { SettingsComponent } from './settings.component'

export const SETTINGS_ROUTES: Routes = [
  {
    path: '',
    component: SettingsComponent,
    children: [
      {
        path: 'email',
        loadComponent: () =>
          import('./routes/email/email.component').then(
            m => m.SettingsEmailComponent,
          ),
      },
      {
        path: 'experimental',
        loadComponent: () =>
          import('./routes/experimental/experimental.component').then(
            m => m.SettingsExperimentalComponent,
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
        path: 'interfaces',
        loadComponent: () =>
          import('./routes/interfaces/interfaces.component').then(
            m => m.SettingsInterfacesComponent,
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
