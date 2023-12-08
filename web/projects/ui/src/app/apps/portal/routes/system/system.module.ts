import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { systemTabResolver } from '../../utils/system-tab-resolver'
import { toNavigationItem } from '../../utils/to-navigation-item'

const ROUTES: Routes = [
  {
    title: systemTabResolver,
    path: 'backups',
    loadComponent: () =>
      import('./backups/backups.component').then(m => m.BackupsComponent),
    data: toNavigationItem('/portal/system/backups'),
  },
  {
    title: systemTabResolver,
    path: 'settings',
    loadChildren: () =>
      import('./settings/settings.routes').then(m => m.SETTINGS_ROUTES),
    data: toNavigationItem('/portal/system/settings'),
  },
  {
    title: systemTabResolver,
    path: 'notifications',
    loadComponent: () =>
      import('./notifications/notifications.component').then(
        m => m.NotificationsComponent,
      ),
    data: toNavigationItem('/portal/system/notifications'),
  },
  {
    title: systemTabResolver,
    path: 'sideload',
    loadComponent: () =>
      import('./sideload/sideload.component').then(m => m.SideloadComponent),
    data: toNavigationItem('/portal/system/sideload'),
  },
  {
    title: systemTabResolver,
    path: 'updates',
    loadComponent: () =>
      import('./updates/updates.component').then(m => m.UpdatesComponent),
    data: toNavigationItem('/portal/system/updates'),
  },
  {
    title: systemTabResolver,
    path: 'snek',
    loadComponent: () =>
      import('./snek/snek.component').then(m => m.SnekComponent),
    data: toNavigationItem('/portal/system/snek'),
  },
]

@NgModule({ imports: [RouterModule.forChild(ROUTES)] })
export class SystemModule {}
