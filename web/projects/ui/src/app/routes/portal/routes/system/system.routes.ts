import { inject } from '@angular/core'
import {
  ActivatedRouteSnapshot,
  Router,
  RouterStateSnapshot,
  Routes,
} from '@angular/router'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { titleResolver } from 'src/app/utils/title-resolver'
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
        title: titleResolver,
        loadComponent: () => import('./routes/general/general.component'),
      },
      {
        path: 'email',
        title: titleResolver,
        loadComponent: () => import('./routes/email/email.component'),
      },
      {
        path: 'backup',
        title: titleResolver,
        loadComponent: () => import('./routes/backups/backups.component'),
        data: { type: 'create' },
      },
      {
        path: 'restore',
        title: titleResolver,
        loadComponent: () => import('./routes/backups/backups.component'),
        data: { type: 'restore' },
      },
      {
        path: 'interfaces',
        title: titleResolver,
        loadComponent: () => import('./routes/startos-ui/startos-ui.component'),
      },
      {
        path: 'acme',
        title: titleResolver,
        loadComponent: () => import('./routes/acme/acme.component'),
      },
      {
        path: 'wifi',
        title: titleResolver,
        loadComponent: () => import('./routes/wifi/wifi.component'),
      },
      {
        path: 'sessions',
        title: titleResolver,
        loadComponent: () => import('./routes/sessions/sessions.component'),
      },
      {
        path: 'password',
        title: titleResolver,
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
