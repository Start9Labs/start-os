import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { TuiLoader, TuiNotification } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { SafeLinksDirective } from '../../directives/safe-links.directive'

import { MarkdownPipeModule } from '../../pipes/markdown/markdown.module'
import { MarkdownComponent } from './markdown.component'

@NgModule({
  declarations: [MarkdownComponent],
  imports: [
    CommonModule,
    MarkdownPipeModule,
    SafeLinksDirective,
    NgDompurifyModule,
    TuiLoader,
    TuiNotification,
  ],
  exports: [MarkdownComponent],
})
export class MarkdownModule {}
