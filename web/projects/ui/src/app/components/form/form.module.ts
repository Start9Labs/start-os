import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { MaskitoModule } from '@maskito/angular'
import { TuiMapperPipeModule, TuiValueChangesModule } from '@taiga-ui/cdk'
import {
  TuiErrorModule,
  TuiExpandModule,
  TuiHintModule,
  TuiLinkModule,
  TuiModeModule,
  TuiTextfieldControllerModule,
  TuiTooltipModule,
} from '@taiga-ui/core'
import {
  TuiAppearanceModule,
  TuiButtonModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import {
  TuiElasticContainerModule,
  TuiFieldErrorPipeModule,
  TuiInputDateModule,
  TuiInputDateTimeModule,
  TuiInputFilesModule,
  TuiInputModule,
  TuiInputNumberModule,
  TuiInputTimeModule,
  TuiMultiSelectModule,
  TuiPromptModule,
  TuiSelectModule,
  TuiTagModule,
  TuiTextareaModule,
  TuiToggleModule,
} from '@taiga-ui/kit'

import { FormGroupComponent } from './form-group/form-group.component'
import { FormTextComponent } from './form-text/form-text.component'
import { FormToggleComponent } from './form-toggle/form-toggle.component'
import { FormTextareaComponent } from './form-textarea/form-textarea.component'
import { FormNumberComponent } from './form-number/form-number.component'
import { FormSelectComponent } from './form-select/form-select.component'
import { FormMultiselectComponent } from './form-multiselect/form-multiselect.component'
import { FormUnionComponent } from './form-union/form-union.component'
import { FormObjectComponent } from './form-object/form-object.component'
import { FormArrayComponent } from './form-array/form-array.component'
import { FormControlComponent } from './form-control/form-control.component'
import { MustachePipe } from './mustache.pipe'
import { ControlDirective } from './control.directive'
import { FormColorComponent } from './form-color/form-color.component'
import { FormDatetimeComponent } from './form-datetime/form-datetime.component'
import { HintPipe } from './hint.pipe'
import { FilterHiddenPipe } from './filter-hidden.pipe'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TuiInputModule,
    TuiInputNumberModule,
    TuiInputFilesModule,
    TuiTextareaModule,
    TuiSelectModule,
    TuiMultiSelectModule,
    TuiToggleModule,
    TuiTooltipModule,
    TuiHintModule,
    TuiModeModule,
    TuiTagModule,
    TuiButtonModule,
    TuiExpandModule,
    TuiTextfieldControllerModule,
    TuiLinkModule,
    TuiPromptModule,
    TuiErrorModule,
    TuiFieldErrorPipeModule,
    TuiValueChangesModule,
    TuiElasticContainerModule,
    MaskitoModule,
    TuiIconModule,
    TuiAppearanceModule,
    TuiInputDateModule,
    TuiInputTimeModule,
    TuiInputDateTimeModule,
    TuiMapperPipeModule,
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
    FormUnionComponent,
    FormObjectComponent,
    FormArrayComponent,
    MustachePipe,
    HintPipe,
    ControlDirective,
    FilterHiddenPipe,
  ],
  exports: [FormGroupComponent],
})
export class FormModule {}
