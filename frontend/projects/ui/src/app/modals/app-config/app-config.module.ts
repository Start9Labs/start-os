import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiButtonModule,
  TuiLoaderModule,
  TuiModeModule,
  TuiNotificationModule,
} from '@taiga-ui/core'
import { AppConfigPage } from './app-config.page'
import { FormPageModule } from '../form/form.module'
import { AppConfigDepComponent } from './app-config-dep.component'

@NgModule({
  imports: [
    CommonModule,
    ReactiveFormsModule,
    FormPageModule,
    TuiLoaderModule,
    TuiNotificationModule,
    TuiButtonModule,
    TuiModeModule,
  ],
  declarations: [AppConfigPage, AppConfigDepComponent],
  exports: [AppConfigPage],
})
export class AppConfigPageModule {}
