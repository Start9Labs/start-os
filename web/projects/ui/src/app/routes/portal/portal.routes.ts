import { Routes } from '@angular/router'
import { SYSTEM_UTILITIES } from 'src/app/utils/system-utilities'
import { titleResolver } from 'src/app/utils/title-resolver'
import { toRouterLink } from 'src/app/utils/to-router-link'
import { PortalComponent } from './portal.component'

const ROUTES: Routes = [
  {
    path: '',
    component: PortalComponent,
    children: [
      {
        redirectTo: 'services',
        pathMatch: 'full',
        path: '',
      },
      {
        path: 'services',
        data: { title: 'Services' },
        title: titleResolver,
        loadChildren: () => import('./routes/services/services.routes'),
      },
      // @TODO 041
      // {
      //   title: titleResolver,
      //   path: 'backups',
      //   loadComponent: () => import('./routes/backups/backups.component'),
      //   data: toNavigationItem('/portal/backups'),
      // },
      {
        title: titleResolver,
        path: 'logs',
        loadComponent: () => import('./routes/logs/logs.component'),
        data: toNavigationItem('/portal/logs'),
      },
      {
        title: titleResolver,
        path: 'marketplace',
        loadChildren: () => import('./routes/marketplace/marketplace.routes'),
        data: toNavigationItem('/portal/marketplace'),
      },
      {
        title: titleResolver,
        path: 'system',
        loadChildren: () => import('./routes/system/system.routes'),
        data: toNavigationItem('/portal/system'),
      },
      {
        title: titleResolver,
        path: 'notifications',
        loadComponent: () =>
          import('./routes/notifications/notifications.component'),
        data: toNavigationItem('/portal/notifications'),
      },
      {
        title: titleResolver,
        path: 'sideload',
        loadComponent: () => import('./routes/sideload/sideload.component'),
        data: toNavigationItem('/portal/sideload'),
      },
      {
        title: titleResolver,
        path: 'updates',
        loadComponent: () => import('./routes/updates/updates.component'),
        data: toNavigationItem('/portal/updates'),
      },
      {
        title: titleResolver,
        path: 'metrics',
        loadComponent: () => import('./routes/metrics/metrics.component'),
        data: toNavigationItem('/portal/metrics'),
      },
    ],
  },
]

export default ROUTES

function toNavigationItem(id: string) {
  const { icon, title } = SYSTEM_UTILITIES[id] || {}

  return {
    icon,
    title,
    routerLink: toRouterLink(id),
  }
}
