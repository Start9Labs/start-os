import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiValueChangesModule } from '@taiga-ui/cdk'
import {
  TuiButtonModule,
  TuiLoaderModule,
  TuiModeModule,
  TuiNotificationModule,
} from '@taiga-ui/core'
import { FormModule } from 'src/app/components/form/form.module'
import { AppConfigPage } from './app-config.page'

@NgModule({
  declarations: [AppConfigPage],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    FormModule,
    TuiLoaderModule,
    TuiButtonModule,
    TuiModeModule,
    TuiNotificationModule,
    TuiValueChangesModule,
  ],
  exports: [AppConfigPage],
})
export class AppConfigPageModule {}
