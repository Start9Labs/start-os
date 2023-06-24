import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { MarkdownModule, ResponsiveColModule } from '@start9labs/shared'

import { AdditionalComponent } from './additional.component'
import {
  TuiRadioListModule,
  TuiStringifyContentPipeModule,
} from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { TuiButtonModule } from '@taiga-ui/core'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    MarkdownModule,
    ResponsiveColModule,
    TuiRadioListModule,
    FormsModule,
    TuiStringifyContentPipeModule,
    TuiButtonModule,
  ],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}
