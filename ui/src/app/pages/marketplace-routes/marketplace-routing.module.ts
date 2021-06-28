import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'browse',
    pathMatch: 'full',
  },
  {
    path: 'browse',
    loadChildren: () => import('./marketplace-list/marketplace-list.module').then(m => m.MarketplaceListPageModule),
  },
  {
    path: ':pkgId',
    loadChildren: () => import('./marketplace-show/marketplace-show.module').then(m => m.MarketplaceShowPageModule),
  },
  {
    path: ':pkgId/notes',
    loadChildren: () => import('./app-release-notes/app-release-notes.module').then(m => m.ReleaseNotesModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class MarketplaceRoutingModule { }