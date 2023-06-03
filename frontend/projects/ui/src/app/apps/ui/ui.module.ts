import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

const ROUTES: Routes = [
  {
    redirectTo: 'services',
    pathMatch: 'full',
    path: '',
  },
  {
    path: 'home',
    loadChildren: () =>
      import('./pages/home/home.module').then(m => m.HomePageModule),
  },
  {
    path: 'system',
    loadChildren: () =>
      import('./pages/system/system.module').then(m => m.SystemModule),
  },
  {
    path: 'updates',
    loadChildren: () =>
      import('./pages/updates/updates.module').then(m => m.UpdatesPageModule),
  },
  {
    path: 'marketplace',
    loadChildren: () =>
      import('./pages/marketplace/marketplace.module').then(
        m => m.MarketplaceModule,
      ),
  },
  {
    path: 'notifications',
    loadChildren: () =>
      import('./pages/notifications/notifications.module').then(
        m => m.NotificationsPageModule,
      ),
  },
  {
    path: 'services',
    loadChildren: () =>
      import('./pages/services/services.module').then(m => m.ServicesModule),
  },
  {
    path: 'backups',
    loadChildren: () =>
      import('./pages/backups/backups.module').then(m => m.BackupsModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
})
export class UiModule {}
