import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiElasticContainerModule } from '@taiga-ui/kit'
import { TuiExpandModule } from '@taiga-ui/core'
import { FormLabelComponent } from './form-label/form-label.component'
import { FormObjectComponent } from './form-object/form-object.component'
import { FormUnionComponent } from './form-union/form-union.component'
import {
  GetErrorPipe,
  ToWarningTextPipe,
  ToElementIdPipe,
  ToRangePipe,
} from './form-object.pipes'
import { FormFileComponent } from './form-object/controls/form-file/form-file.component'
import { FormInputComponent } from './form-object/controls/form-input/form-input.component'
import { FormWarningDirective } from './form-warning.directive'
import { FormSubformComponent } from './form-object/controls/form-subform/form-subform.component'
import { FormSelectComponent } from './form-object/controls/form-select/form-select.component'

@NgModule({
  declarations: [
    FormObjectComponent,
    FormUnionComponent,
    FormLabelComponent,
    ToWarningTextPipe,
    GetErrorPipe,
    ToElementIdPipe,
    ToRangePipe,
    FormWarningDirective,
    FormFileComponent,
    FormInputComponent,
    FormSubformComponent,
    FormSelectComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ReactiveFormsModule,
    SharedPipesModule,
    TuiElasticContainerModule,
    TuiExpandModule,
  ],
  exports: [FormObjectComponent, FormLabelComponent],
})
export class FormObjectModule {}
