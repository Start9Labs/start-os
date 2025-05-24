import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { MarkdownPipe, SafeLinksDirective } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { TuiTagModule } from '@taiga-ui/legacy'
import { AboutComponent } from './about.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    TuiTagModule,
    NgDompurifyPipe,
    SafeLinksDirective,
    MarkdownPipe,
    TuiButton,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
