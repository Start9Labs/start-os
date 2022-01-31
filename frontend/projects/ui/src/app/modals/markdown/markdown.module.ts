import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarkdownPage } from './markdown.page'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [MarkdownPage],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [MarkdownPage],
})
export class MarkdownPageModule { }
