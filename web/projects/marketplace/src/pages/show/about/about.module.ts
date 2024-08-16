import { TuiButton } from '@taiga-ui/core'
import { TuiTagModule } from '@taiga-ui/legacy'
import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { AboutComponent } from './about.component'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { MarkdownPipeModule, SafeLinksDirective } from '@start9labs/shared'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    TuiTagModule,
    NgDompurifyModule,
    SafeLinksDirective,
    MarkdownPipeModule,
    TuiButton,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
