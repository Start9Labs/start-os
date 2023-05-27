import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SideloadPage } from './sideload.page'
import { Routes, RouterModule } from '@angular/router'
import { EmverPipesModule, SharedPipesModule } from '@start9labs/shared'
import { DragNDropDirective } from './dnd.directive'
import {
  PackageModule,
  AboutModule,
  AdditionalModule,
  DependenciesModule,
} from '@start9labs/marketplace'
import { MarketplaceShowComponentsModule } from '../../marketplace-routes/marketplace-show/components/marketplace-show-components.module'

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
  ],
  declarations: [SideloadPage, DragNDropDirective],
})
export class SideloadPageModule {}
