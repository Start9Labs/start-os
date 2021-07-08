import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarkdownPage } from './markdown.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { TextSpinnerComponentModule } from 'src/app/components/text-spinner/text-spinner.component.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    TextSpinnerComponentModule,
  ],
  declarations: [MarkdownPage],
})
export class MarkdownPageModule { }
