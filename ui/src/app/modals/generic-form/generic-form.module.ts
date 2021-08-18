import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { GenericFormPage } from './generic-form.page'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { FormObjectComponentModule } from 'src/app/components/form-object/form-object.component.module'

@NgModule({
  declarations: [GenericFormPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    FormObjectComponentModule,
  ],
  entryComponents: [GenericFormPage],
  exports: [GenericFormPage],
})
export class GenericFormPageModule { }