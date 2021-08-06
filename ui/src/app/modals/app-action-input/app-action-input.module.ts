import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppActionInputPage } from './app-action-input.page'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { FormObjectComponentModule } from 'src/app/components/form-object/form-object.component.module'

@NgModule({
  declarations: [AppActionInputPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    FormObjectComponentModule,
  ],
  entryComponents: [AppActionInputPage],
  exports: [AppActionInputPage],
})
export class AppActionInputPageModule { }