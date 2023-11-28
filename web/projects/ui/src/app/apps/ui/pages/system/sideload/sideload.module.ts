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
  MarketplacePackageHeroComponent,
} from '@start9labs/marketplace'
// TODO: Find a way to not tie two routes together
import { MarketplaceShowControlsComponent } from '../../marketplace/marketplace-show-preview/components/marketplace-show-controls.component'

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
    AboutModule,
    AdditionalModule,
    MarketplaceShowControlsComponent,
    DependenciesModule,
    MarketplacePackageHeroComponent,
  ],
  declarations: [SideloadPage, DragNDropDirective],
})
export class SideloadPageModule {}
