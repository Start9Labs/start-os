import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarkdownPage } from './markdown.page'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'

@NgModule({
  declarations: [MarkdownPage],
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    TextSpinnerComponentModule,
  ],
  exports: [MarkdownPage],
})
export class MarkdownPageModule {}
