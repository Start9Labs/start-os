import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { PackageModule } from '@start9labs/marketplace'
import { MarketplaceSidebarModule } from '../components/marketplace-sidebar/marketplace-sidebar.module'
import { MarketplaceShowPage } from './marketplace-show.page'
import { MarketplaceShowComponentsModule } from './components/marketplace-show-components.module'

const routes: Routes = [
  {
    path: '',
    component: MarketplaceShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    SharedPipesModule,
    MarketplaceSidebarModule,
    PackageModule,
    MarketplaceShowComponentsModule,
  ],
  declarations: [MarketplaceShowPage],
})
export class MarketplaceShowPageModule {}
