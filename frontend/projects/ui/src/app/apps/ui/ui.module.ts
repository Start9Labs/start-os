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
      import('./pages/server-routes/server-routing.module').then(
        m => m.ServerRoutingModule,
      ),
  },
  {
    path: 'updates',
    loadChildren: () =>
      import('./pages/updates/updates.module').then(m => m.UpdatesPageModule),
  },
  {
    path: 'marketplace',
    loadChildren: () =>
      import('./pages/marketplace-routes/marketplace-routing.module').then(
        m => m.MarketplaceRoutingModule,
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
      import('./pages/apps-routes/apps-routing.module').then(
        m => m.AppsRoutingModule,
      ),
  },
  {
    path: 'backups',
    loadChildren: () =>
      import('./pages/backups-routes/backups-routing.module').then(
        m => m.BackupsRoutingModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
})
export class UiModule {}
