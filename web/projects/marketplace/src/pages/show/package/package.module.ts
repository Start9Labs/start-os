import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

import { PackageComponent } from './package.component'
import { MimeTypePipeModule } from '../../../pipes/mime-type.pipe'
import { DependenciesModule } from '../dependencies/dependencies.module'
import { AdditionalModule } from '../additional/additional.module'
import { TuiCarouselModule } from '@taiga-ui/kit'
import { TuiButtonModule } from '@taiga-ui/core'
import { ReleaseNotesModule } from '../../release-notes/release-notes.module'
import { RouterModule } from '@angular/router'
import { AboutModule } from '../about/about.module'
import { MarketplaceHeaderModule } from '../../../components/header/header.component.module'

@NgModule({
  declarations: [PackageComponent],
  exports: [PackageComponent],
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    EmverPipesModule,
    MimeTypePipeModule,
    TextSpinnerComponentModule,
    RouterModule,
    DependenciesModule,
    AdditionalModule,
    ReleaseNotesModule,
    TuiCarouselModule,
    TuiButtonModule,
    AboutModule,
    MarketplaceHeaderModule,
  ],
})
export class PackageModule {}
