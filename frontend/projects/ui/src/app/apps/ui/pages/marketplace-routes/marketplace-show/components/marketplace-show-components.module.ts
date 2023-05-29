import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
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
import { MarketplaceShowHeaderComponent } from './marketplace-show-header/marketplace-show-header.component'
import { MarketplaceShowDependentComponent } from './marketplace-show-dependent/marketplace-show-dependent.component'
import { MarketplaceShowControlsComponent } from './marketplace-show-controls/marketplace-show-controls.component'

@NgModule({
  declarations: [
    MarketplaceShowHeaderComponent,
    MarketplaceShowControlsComponent,
    MarketplaceShowDependentComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule,
    TextSpinnerComponentModule,
    SharedPipesModule,
    EmverPipesModule,
    MarkdownPipeModule,
    PackageModule,
    AboutModule,
    DependenciesModule,
    AdditionalModule,
  ],
  exports: [
    MarketplaceShowHeaderComponent,
    MarketplaceShowControlsComponent,
    MarketplaceShowDependentComponent,
  ],
})
export class MarketplaceShowComponentsModule {}
