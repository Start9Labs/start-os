import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { TuiElementModule } from '@taiga-ui/cdk'

import { ReleaseNotesComponent } from './release-notes.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TextSpinnerComponentModule,
    EmverPipesModule,
    MarkdownPipeModule,
    TuiElementModule,
  ],
  declarations: [ReleaseNotesComponent],
  exports: [ReleaseNotesComponent],
})
export class ReleaseNotesModule {}
