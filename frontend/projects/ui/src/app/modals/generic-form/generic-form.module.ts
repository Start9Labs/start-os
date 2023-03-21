import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { GenericFormPage } from './generic-form.page'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { FormObjectModule } from 'src/app/components/form-object/form-object.module'

@NgModule({
  declarations: [GenericFormPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    FormObjectModule,
  ],
  exports: [GenericFormPage],
})
export class GenericFormPageModule {}
