import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { AppConfigPage } from './app-config.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormObjectComponentModule } from 'src/app/components/form-object/form-object.component.module'

@NgModule({
  declarations: [AppConfigPage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    SharingModule,
    FormObjectComponentModule,
    ReactiveFormsModule,
  ],
  exports: [AppConfigPage],
})
export class AppConfigPageModule { }
