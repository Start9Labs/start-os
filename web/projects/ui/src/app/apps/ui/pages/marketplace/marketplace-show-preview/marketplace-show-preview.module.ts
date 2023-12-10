import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import {
  AboutModule,
  AdditionalModule,
  DependenciesModule,
  MarketplacePackageHeroComponent,
  ReleaseNotesModule,
} from '@start9labs/marketplace'
import { MarketplaceShowPreviewComponent } from './marketplace-show-preview.component'

import { TuiButtonModule } from '@taiga-ui/experimental'
import { RouterModule } from '@angular/router'

@NgModule({
  declarations: [MarketplaceShowPreviewComponent],
  exports: [MarketplaceShowPreviewComponent],
  imports: [
    CommonModule,
    SharedPipesModule,
    TextSpinnerComponentModule,
    RouterModule,
    DependenciesModule,
    AdditionalModule,
    ReleaseNotesModule,
    TuiButtonModule,
    AboutModule,
    MarketplacePackageHeroComponent,
  ],
})
export class MarketplaceShowPreviewModule {}
