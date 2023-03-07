import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { TuiElementModule } from '@taiga-ui/cdk'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'

import { ReleaseNotesComponent } from './release-notes.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TextSpinnerComponentModule,
    EmverPipesModule,
    MarkdownPipeModule,
    TuiElementModule,
    NgDompurifyModule,
    SafeLinksModule,
  ],
  declarations: [ReleaseNotesComponent],
  exports: [ReleaseNotesComponent],
})
export class ReleaseNotesModule {}
