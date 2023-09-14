import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    loadChildren: () =>
      import('./marketplace-list/marketplace-list.module').then(
        m => m.MarketplaceListPageModule,
      ),
  },
  {
    path: 'updates',
    loadChildren: () =>
      import('./updates/updates.module').then(m => m.UpdatesPageModule),
  },
  {
    path: 'sideload',
    loadChildren: () =>
      import('./sideload/sideload.module').then(m => m.SideloadPageModule),
  },
  {
    path: ':pkgId',
    loadChildren: () =>
      import('./marketplace-show/marketplace-show.module').then(
        m => m.MarketplaceShowPageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class MarketplaceModule {}
