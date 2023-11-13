import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'

import { MarkdownPipeModule } from '../../pipes/markdown/markdown.module'
import { SafeLinksDirective } from '../../directives/safe-links.directive'
import { TextSpinnerComponentModule } from '../text-spinner/text-spinner.component.module'
import { MarkdownComponent } from './markdown.component'

@NgModule({
  declarations: [MarkdownComponent],
  imports: [
    CommonModule,
    IonicModule,
    MarkdownPipeModule,
    TextSpinnerComponentModule,
    SafeLinksDirective,
    NgDompurifyModule,
  ],
  exports: [MarkdownComponent],
})
export class MarkdownModule {}
