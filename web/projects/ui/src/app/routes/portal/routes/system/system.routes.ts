import { inject } from '@angular/core'
import {
  ActivatedRouteSnapshot,
  Router,
  RouterStateSnapshot,
  Routes,
} from '@angular/router'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { SystemComponent } from './system.component'

export default [
  {
    path: '',
    component: SystemComponent,
    canActivate: [
      ({ firstChild }: ActivatedRouteSnapshot, state: RouterStateSnapshot) =>
        !!firstChild ||
        inject(TUI_IS_MOBILE) ||
        inject(Router).parseUrl(`${state.url}/general`),
    ],
    children: [
      {
        path: 'general',
        loadComponent: () => import('./routes/general/general.component'),
      },
      {
        path: 'email',
        loadComponent: () => import('./routes/email/email.component'),
      },
      {
        path: 'backup',
        loadComponent: () => import('./routes/backups/backups.component'),
        data: { type: 'create' },
      },
      {
        path: 'restore',
        loadComponent: () => import('./routes/backups/backups.component'),
        data: { type: 'restore' },
      },
      {
        path: 'interfaces',
        loadComponent: () => import('./routes/interfaces/interfaces.component'),
      },
      {
        path: 'acme',
        loadComponent: () => import('./routes/acme/acme.component'),
      },
      {
        path: 'wifi',
        loadComponent: () => import('./routes/wifi/wifi.component'),
      },
      {
        path: 'sessions',
        loadComponent: () => import('./routes/sessions/sessions.component'),
      },
      {
        path: 'password',
        loadComponent: () => import('./routes/password/password.component'),
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
    ],
  },
] satisfies Routes
