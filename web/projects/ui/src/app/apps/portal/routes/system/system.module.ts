import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { systemTabResolver } from '../../utils/system-tab-resolver'
import { toNavigationItem } from '../../utils/to-navigation-item'

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
    path: 'snek',
    loadComponent: () => import('./snek/snek.component'),
    data: toNavigationItem('/portal/system/snek'),
  },
]

@NgModule({ imports: [RouterModule.forChild(ROUTES)] })
export class SystemModule {}
