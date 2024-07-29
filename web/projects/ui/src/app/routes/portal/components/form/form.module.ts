import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { MaskitoDirective } from '@maskito/angular'
import { TuiMapperPipe, TuiValueChanges } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  TuiError,
  TuiExpand,
  TuiHint,
  TuiIcon,
  TuiLink,
  TuiNumberFormat,
} from '@taiga-ui/core'
import {
  TuiElasticContainer,
  TuiFieldErrorPipe,
  TuiFiles,
  TuiSwitch,
  TuiTooltip,
} from '@taiga-ui/kit'
import {
  TuiInputDateModule,
  TuiInputDateTimeModule,
  TuiInputModule,
  TuiInputNumberModule,
  TuiInputTimeModule,
  TuiMultiSelectModule,
  TuiSelectModule,
  TuiTagModule,
  TuiTextareaModule,
  TuiTextfieldControllerModule,
} from '@taiga-ui/legacy'
import { ControlDirective } from './control.directive'
import { FormArrayComponent } from './form-array/form-array.component'
import { FormColorComponent } from './form-color/form-color.component'
import { FormControlComponent } from './form-control/form-control.component'
import { FormDatetimeComponent } from './form-datetime/form-datetime.component'
import { FormFileComponent } from './form-file/form-file.component'

import { FormGroupComponent } from './form-group/form-group.component'
import { FormMultiselectComponent } from './form-multiselect/form-multiselect.component'
import { FormNumberComponent } from './form-number/form-number.component'
import { FormObjectComponent } from './form-object/form-object.component'
import { FormSelectComponent } from './form-select/form-select.component'
import { FormTextComponent } from './form-text/form-text.component'
import { FormTextareaComponent } from './form-textarea/form-textarea.component'
import { FormToggleComponent } from './form-toggle/form-toggle.component'
import { FormUnionComponent } from './form-union/form-union.component'
import { HintPipe } from './hint.pipe'
import { MustachePipe } from './mustache.pipe'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TuiInputModule,
    TuiInputNumberModule,
    ...TuiFiles,
    TuiTextareaModule,
    TuiSelectModule,
    TuiMultiSelectModule,
    TuiSwitch,
    TuiTooltip,
    ...TuiHint,
    TuiTagModule,
    TuiButton,
    ...TuiExpand,
    TuiTextfieldControllerModule,
    TuiLink,
    TuiError,
    TuiFieldErrorPipe,
    TuiValueChanges,
    TuiElasticContainer,
    MaskitoDirective,
    TuiInputDateModule,
    TuiInputTimeModule,
    TuiInputDateTimeModule,
    TuiMapperPipe,
    TuiAppearance,
    TuiIcon,
    TuiNumberFormat,
  ],
  declarations: [
    FormGroupComponent,
    FormControlComponent,
    FormColorComponent,
    FormDatetimeComponent,
    FormTextComponent,
    FormToggleComponent,
    FormTextareaComponent,
    FormNumberComponent,
    FormSelectComponent,
    FormMultiselectComponent,
    FormFileComponent,
    FormUnionComponent,
    FormObjectComponent,
    FormArrayComponent,
    MustachePipe,
    HintPipe,
    ControlDirective,
  ],
  exports: [FormGroupComponent],
})
export class FormModule {}
