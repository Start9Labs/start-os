import { CommonModule } from '@angular/common'
import { CUSTOM_ELEMENTS_SCHEMA, NgModule } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import {
  TuiErrorModule,
  TuiExpandModule,
  TuiLinkModule,
  TuiScrollbarModule,
  TuiSvgModule,
  TuiTooltipModule,
} from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import {
  TuiElasticContainerModule,
  TuiInputDateModule,
  TuiInputDateTimeModule,
  TuiInputFilesModule,
  TuiInputModule,
  TuiInputNumberModule,
  TuiInputTimeModule,
  TuiMultiSelectModule,
  TuiProgressModule,
  TuiRadioListModule,
  TuiSelectModule,
  TuiTextareaModule,
  TuiToggleModule,
} from '@taiga-ui/kit'
import { QrCodeModule } from 'ng-qrcode'
import { PreloaderComponent } from './preloader.component'

@NgModule({
  imports: [
    CommonModule,
    ReactiveFormsModule,
    IonicModule,
    QrCodeModule,
    TuiTooltipModule,
    TuiErrorModule,
    TuiInputModule,
    TuiSvgModule,
    TuiIconModule,
    TuiButtonModule,
    TuiLinkModule,
    TuiInputTimeModule,
    TuiInputDateModule,
    TuiInputDateTimeModule,
    TuiInputFilesModule,
    TuiMultiSelectModule,
    TuiInputNumberModule,
    TuiExpandModule,
    TuiSelectModule,
    TuiTextareaModule,
    TuiToggleModule,
    TuiElasticContainerModule,
    TuiCellModule,
    TuiProgressModule,
    TuiScrollbarModule,
    TuiRadioListModule,
  ],
  declarations: [PreloaderComponent],
  exports: [PreloaderComponent],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
})
export class PreloaderModule {}
