import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  EmverPipesModule,
  MarkdownPipeModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import {
  PackageModule,
  AboutModule,
  AdditionalModule,
  DependenciesModule,
} from '@start9labs/marketplace'
import { MarketplaceStatusModule } from '../marketplace-status/marketplace-status.module'
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
    EmverPipesModule,
    MarkdownPipeModule,
    MarketplaceStatusModule,
    PackageModule,
    AboutModule,
    DependenciesModule,
    AdditionalModule,
    MarketplaceShowComponentsModule,
  ],
  declarations: [MarketplaceShowPage],
})
export class MarketplaceShowPageModule {}
