import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { MarkdownModule, ResponsiveColModule } from '@start9labs/shared'
import { AdditionalComponent } from './additional.component'
import {
  TuiRadioListModule,
  TuiStringifyContentPipeModule,
  TuiTagModule,
} from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { TuiButtonModule, TuiLabelModule, TuiSvgModule } from '@taiga-ui/core'
import { AdditionalLinkModule } from './additional-link/additional-link.component.module'

@NgModule({
  imports: [
    CommonModule,
    MarkdownModule,
    ResponsiveColModule,
    TuiRadioListModule,
    FormsModule,
    TuiStringifyContentPipeModule,
    TuiButtonModule,
    TuiLabelModule,
    TuiSvgModule,
    TuiTagModule,
    AdditionalLinkModule,
  ],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}
