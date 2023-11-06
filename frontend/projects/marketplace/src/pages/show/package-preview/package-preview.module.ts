import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

import { PackagePreviewComponent } from './package-preview.component'
import { MimeTypePipeModule } from '../../../pipes/mime-type.pipe'
import { DependenciesModule } from '../dependencies/dependencies.module'
import { AdditionalModule } from '../additional/additional.module'
import { TuiCarouselModule } from '@taiga-ui/kit'
import { TuiButtonModule } from '@taiga-ui/core'
import { ReleaseNotesModule } from '../../release-notes/release-notes.module'
import { RouterModule } from '@angular/router'
import { AboutModule } from '../about/about.module'

@NgModule({
  declarations: [PackagePreviewComponent],
  exports: [PackagePreviewComponent],
  imports: [
    CommonModule,
    SharedPipesModule,
    MimeTypePipeModule,
    TextSpinnerComponentModule,
    RouterModule,
    DependenciesModule,
    AdditionalModule,
    ReleaseNotesModule,
    TuiCarouselModule,
    TuiButtonModule,
    AboutModule,
  ],
})
export class PackagePreviewModule {}
