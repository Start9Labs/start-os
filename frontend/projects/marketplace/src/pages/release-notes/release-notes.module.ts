import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  TextSpinnerComponentModule,
  ElementModule,
} from '@start9labs/shared'

import { ReleaseNotesComponent } from './release-notes.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TextSpinnerComponentModule,
    EmverPipesModule,
    MarkdownPipeModule,
    ElementModule,
  ],
  declarations: [ReleaseNotesComponent],
  exports: [ReleaseNotesComponent],
})
export class ReleaseNotesModule {}
