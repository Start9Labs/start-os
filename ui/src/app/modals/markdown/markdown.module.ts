import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarkdownPage } from './markdown.page'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  declarations: [MarkdownPage],
})
export class MarkdownPageModule { }
