import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { ReleaseNotesComponent } from './release-notes.component'
import { TuiButtonModule } from '@taiga-ui/core'
import { TuiAccordionModule } from '@taiga-ui/kit'

@NgModule({
  imports: [
    CommonModule,
    TextSpinnerComponentModule,
    EmverPipesModule,
    MarkdownPipeModule,
    NgDompurifyModule,
    SafeLinksModule,
    TuiButtonModule,
    TuiAccordionModule,
  ],
  declarations: [ReleaseNotesComponent],
  exports: [ReleaseNotesComponent],
})
export class ReleaseNotesModule {}
