import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SideloadPage } from './sideload.page'
import { RouterModule, Routes } from '@angular/router'
import { EmverPipesModule, SharedPipesModule } from '@start9labs/shared'
import { DragNDropDirective } from './dnd.directive'
import {
  AboutModule,
  AdditionalModule,
  DependenciesModule,
  PackageModule,
} from '@start9labs/marketplace'
// TODO: Find a way to not tie two routes together
import { MarketplaceShowComponentsModule } from '../../marketplace/marketplace-show/components/marketplace-show-components.module'
import { MarketplaceSidebarModule } from '../components/marketplace-sidebar/marketplace-sidebar.module'

const routes: Routes = [
  {
    path: '',
    component: SideloadPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    EmverPipesModule,
    PackageModule,
    AboutModule,
    AdditionalModule,
    DependenciesModule,
    MarketplaceShowComponentsModule,
    MarketplaceSidebarModule,
  ],
  declarations: [SideloadPage, DragNDropDirective],
})
export class SideloadPageModule {}
