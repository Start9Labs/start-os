import { ActivatedRouteSnapshot, Routes } from '@angular/router'
import { PackageDataEntry } from '../../services/patch-db/data-model'
import { getManifest } from '../../utils/get-package-data'
import { SYSTEM_UTILITIES } from '../../utils/system-utilities'
import { toRouterLink } from '../../utils/to-router-link'
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
        loadChildren: () => import('./routes/services/services.routes'),
      },
      // @TODO 041
      // {
      //   title: systemTabResolver,
      //   path: 'backups',
      //   loadComponent: () => import('./routes/backups/backups.component'),
      //   data: toNavigationItem('/portal/backups'),
      // },
      {
        title: systemTabResolver,
        path: 'logs',
        loadComponent: () => import('./routes/logs/logs.component'),
        data: toNavigationItem('/portal/logs'),
      },
      {
        title: systemTabResolver,
        path: 'marketplace',
        loadChildren: () => import('./routes/marketplace/marketplace.routes'),
        data: toNavigationItem('/portal/marketplace'),
      },
      {
        title: systemTabResolver,
        path: 'system',
        loadChildren: () => import('./routes/system/system.routes'),
        data: toNavigationItem('/portal/system'),
      },
      {
        title: systemTabResolver,
        path: 'notifications',
        loadComponent: () =>
          import('./routes/notifications/notifications.component'),
        data: toNavigationItem('/portal/notifications'),
      },
      {
        title: systemTabResolver,
        path: 'sideload',
        loadComponent: () => import('./routes/sideload/sideload.component'),
        data: toNavigationItem('/portal/sideload'),
      },
      {
        title: systemTabResolver,
        path: 'updates',
        loadComponent: () => import('./routes/updates/updates.component'),
        data: toNavigationItem('/portal/updates'),
      },
      {
        title: systemTabResolver,
        path: 'metrics',
        loadComponent: () => import('./routes/metrics/metrics.component'),
        data: toNavigationItem('/portal/metrics'),
      },
    ],
  },
]

export default ROUTES

function systemTabResolver({ data }: ActivatedRouteSnapshot): string {
  return data['title']
}

function toNavigationItem(
  id: string,
  packages: Record<string, PackageDataEntry> = {},
) {
  const item = SYSTEM_UTILITIES[id]
  const routerLink = toRouterLink(id)

  return item
    ? {
        icon: item.icon,
        title: item.title,
        routerLink,
      }
    : {
        icon: packages[id]?.icon,
        title: getManifest(packages[id]!).title,
        routerLink,
      }
}
