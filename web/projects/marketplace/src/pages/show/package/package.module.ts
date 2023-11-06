import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

import { PackageComponent } from './package.component'
import { DependenciesModule } from '../dependencies/dependencies.module'
import { AdditionalModule } from '../additional/additional.module'
import { TuiButtonModule } from '@taiga-ui/core'
import { ReleaseNotesModule } from '../../release-notes/release-notes.module'
import { RouterModule } from '@angular/router'
import { AboutModule } from '../about/about.module'
import { MarketplacePackageScreenshotComponent } from '../screenshots/screenshots.component'
import { MarketplacePackageHeroComponent } from '../hero/hero.component'

@NgModule({
  declarations: [PackageComponent],
  exports: [PackageComponent],
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
    MarketplacePackageScreenshotComponent,
    MarketplacePackageHeroComponent,
  ],
})
export class PackageModule {}
