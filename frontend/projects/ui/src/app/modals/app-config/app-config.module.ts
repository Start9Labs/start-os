import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { AppConfigPage } from './app-config.page'
import { TextSpinnerComponentModule } from '@start9labs/shared'
import { FormObjectModule } from 'src/app/components/form-object/form-object.module'

@NgModule({
  declarations: [AppConfigPage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    TextSpinnerComponentModule,
    FormObjectModule,
    ReactiveFormsModule,
  ],
  exports: [AppConfigPage],
})
export class AppConfigPageModule {}
