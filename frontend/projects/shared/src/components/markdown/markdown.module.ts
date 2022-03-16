import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'

import { MarkdownPipeModule } from '../../pipes/markdown/markdown.module'
import { TextSpinnerComponentModule } from '../text-spinner/text-spinner.component.module'
import { MarkdownComponent } from './markdown.component'

@NgModule({
  declarations: [MarkdownComponent],
  imports: [
    CommonModule,
    IonicModule,
    MarkdownPipeModule,
    TextSpinnerComponentModule,
  ],
  exports: [MarkdownComponent],
})
export class MarkdownModule {}
