import { NgModule } from '@angular/core'
import { ActivatedRouteSnapshot, RouterModule, Routes } from '@angular/router'
import { toRouterLink } from 'src/app/utils/to-router-link'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import { SYSTEM_UTILITIES } from 'src/app/utils/system-utilities'

const ROUTES: Routes = [
  {
    title: systemTabResolver,
    path: 'backups',
    loadComponent: () => import('./backups/backups.component'),
    data: toNavigationItem('/portal/system/backups'),
  },
  {
    title: systemTabResolver,
    path: 'logs',
    loadComponent: () => import('./logs/logs.component'),
    data: toNavigationItem('/portal/system/logs'),
  },
  {
    title: systemTabResolver,
    path: 'marketplace',
    loadChildren: () => import('./marketplace/marketplace.routes'),
    data: toNavigationItem('/portal/system/marketplace'),
  },
  {
    title: systemTabResolver,
    path: 'settings',
    loadChildren: () => import('./settings/settings.routes'),
    data: toNavigationItem('/portal/system/settings'),
  },
  {
    title: systemTabResolver,
    path: 'notifications',
    loadComponent: () => import('./notifications/notifications.component'),
    data: toNavigationItem('/portal/system/notifications'),
  },
  {
    title: systemTabResolver,
    path: 'sideload',
    loadComponent: () => import('./sideload/sideload.component'),
    data: toNavigationItem('/portal/system/sideload'),
  },
  {
    title: systemTabResolver,
    path: 'updates',
    loadComponent: () => import('./updates/updates.component'),
    data: toNavigationItem('/portal/system/updates'),
  },
  {
    title: systemTabResolver,
    path: 'metrics',
    loadComponent: () => import('./metrics/metrics.component'),
    data: toNavigationItem('/portal/system/metrics'),
  },
]

@NgModule({ imports: [RouterModule.forChild(ROUTES)] })
export class SystemModule {}

function systemTabResolver({ data }: ActivatedRouteSnapshot): string {
  return data['title']
}

function toNavigationItem(
  id: string,
  packages: Record<string, PackageDataEntry> = {},
) {
  const item = SYSTEM_UTILITIES[id]
  const routerLink = toRouterLink(id)

  return SYSTEM_UTILITIES[id]
    ? {
        icon: item.icon,
        title: item.title,
        routerLink,
      }
    : {
        icon: packages[id]?.icon,
        title: getManifest(packages[id]).title,
        routerLink,
      }
}
