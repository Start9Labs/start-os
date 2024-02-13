import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiLoaderModule, TuiNotificationModule } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'

import { MarkdownPipeModule } from '../../pipes/markdown/markdown.module'
import { SafeLinksDirective } from '../../directives/safe-links.directive'
import { MarkdownComponent } from './markdown.component'

@NgModule({
  declarations: [MarkdownComponent],
  imports: [
    CommonModule,
    MarkdownPipeModule,
    SafeLinksDirective,
    NgDompurifyModule,
    TuiLoaderModule,
    TuiNotificationModule,
  ],
  exports: [MarkdownComponent],
})
export class MarkdownModule {}
