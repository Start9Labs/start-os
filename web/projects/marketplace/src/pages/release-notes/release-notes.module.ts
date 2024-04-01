import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiAccordionModule } from '@taiga-ui/kit'
import { TuiButtonModule, TuiLoaderModule } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import {
  FilterVersionsPipe,
  ReleaseNotesComponent,
} from './release-notes.component'

@NgModule({
  imports: [
    CommonModule,
    EmverPipesModule,
    MarkdownPipeModule,
    NgDompurifyModule,
    SafeLinksDirective,
    TuiButtonModule,
    TuiAccordionModule,
    TuiLoaderModule,
    FilterVersionsPipe,
  ],
  declarations: [ReleaseNotesComponent],
  exports: [ReleaseNotesComponent],
})
export class ReleaseNotesModule {}
