import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiValueChangesModule } from '@taiga-ui/cdk'
import { TuiButtonModule, TuiModeModule } from '@taiga-ui/core'
import { FormModule } from 'src/app/common/form/form.module'
import { FormPage } from './form.page'

@NgModule({
  imports: [
    CommonModule,
    ReactiveFormsModule,
    TuiValueChangesModule,
    TuiButtonModule,
    TuiModeModule,
    FormModule,
  ],
  declarations: [FormPage],
  exports: [FormPage],
})
export class FormPageModule {}
