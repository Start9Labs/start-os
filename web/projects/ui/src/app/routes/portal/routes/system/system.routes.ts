import { SystemComponent } from './system.component'

export default [
  {
    path: '',
    component: SystemComponent,
    children: [
      {
        path: 'acme',
        loadComponent: () => import('./routes/acme/acme.component'),
      },
      {
        path: 'email',
        loadComponent: () => import('./routes/email/email.component'),
      },
      // {
      //   path: 'domains',
      //   loadComponent: () => import('./routes/domains/domains.component')
      // },
      // {
      //   path: 'proxies',
      //   loadComponent: () => import('./routes/proxies/proxies.component')
      // },
      // {
      //   path: 'router',
      //   loadComponent: () => import('./routes/router/router.component')
      // },
      {
        path: 'wifi',
        loadComponent: () => import('./routes/wifi/wifi.component'),
      },
      {
        path: 'ui',
        loadComponent: () => import('./routes/interfaces/ui.component'),
      },
      {
        path: 'ssh',
        loadComponent: () => import('./routes/ssh/ssh.component'),
      },
      {
        path: 'sessions',
        loadComponent: () => import('./routes/sessions/sessions.component'),
      },
    ],
  },
]
