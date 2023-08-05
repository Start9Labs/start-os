import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import {
  EmverPipesModule,
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
import { MarketplaceShowHeaderComponent } from './marketplace-show-header/marketplace-show-header.component'
import { MarketplaceShowDependentComponent } from './marketplace-show-dependent/marketplace-show-dependent.component'
import { MarketplaceShowControlsComponent } from './marketplace-show-controls/marketplace-show-controls.component'
import { TuiButtonModule } from '@taiga-ui/core'

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
    TuiButtonModule,
  ],
  exports: [
    MarketplaceShowHeaderComponent,
    MarketplaceShowControlsComponent,
    MarketplaceShowDependentComponent,
  ],
})
export class MarketplaceShowComponentsModule {}
