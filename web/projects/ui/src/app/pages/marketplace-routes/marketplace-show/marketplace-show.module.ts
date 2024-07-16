import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  ExverPipesModule,
  MarkdownPipeModule,
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import {
  AboutModule,
  AdditionalModule,
  DependenciesModule,
  PackageModule,
} from '@start9labs/marketplace'
import { MarketplaceStatusModule } from '../marketplace-status/marketplace-status.module'
import { MarketplaceShowPage } from './marketplace-show.page'
import { MarketplaceShowHeaderComponent } from './marketplace-show-header/marketplace-show-header.component'
import { MarketplaceShowDependentComponent } from './marketplace-show-dependent/marketplace-show-dependent.component'
import { MarketplaceShowControlsComponent } from './marketplace-show-controls/marketplace-show-controls.component'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'

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
    ExverPipesModule,
    MarkdownPipeModule,
    MarketplaceStatusModule,
    PackageModule,
    AboutModule,
    DependenciesModule,
    AdditionalModule,
    UiPipeModule,
  ],
  declarations: [
    MarketplaceShowPage,
    MarketplaceShowHeaderComponent,
    MarketplaceShowControlsComponent,
    MarketplaceShowDependentComponent,
  ],
  exports: [
    MarketplaceShowPage,
    MarketplaceShowHeaderComponent,
    MarketplaceShowControlsComponent,
    MarketplaceShowDependentComponent,
  ],
})
export class MarketplaceShowPageModule {}
