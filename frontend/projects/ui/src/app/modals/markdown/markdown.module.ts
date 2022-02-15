import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarkdownPage } from './markdown.page'
import {
  MarkdownPipeModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

@NgModule({
  declarations: [MarkdownPage],
  imports: [
    CommonModule,
    IonicModule,
    MarkdownPipeModule,
    TextSpinnerComponentModule,
  ],
  exports: [MarkdownPage],
})
export class MarkdownPageModule {}
