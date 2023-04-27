import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule, ReactiveFormsModule } from '@angular/forms'
import { TuiValueChangesModule } from '@taiga-ui/cdk'
import {
  TuiButtonModule,
  TuiErrorModule,
  TuiExpandModule,
  TuiHintModule,
  TuiLinkModule,
  TuiModeModule,
  TuiSvgModule,
  TuiTextfieldControllerModule,
  TuiTooltipModule,
  TuiWrapperModule,
} from '@taiga-ui/core'
import {
  TuiElasticContainerModule,
  TuiFieldErrorPipeModule,
  TuiInputFilesModule,
  TuiInputModule,
  TuiInputNumberModule,
  TuiMultiSelectModule,
  TuiPromptModule,
  TuiSelectModule,
  TuiTagModule,
  TuiTextAreaModule,
  TuiToggleModule,
} from '@taiga-ui/kit'

import { FormGroupComponent } from './form-group/form-group.component'
import { FormTextComponent } from './form-text/form-text.component'
import { FormToggleComponent } from './form-toggle/form-toggle.component'
import { FormTextareaComponent } from './form-textarea/form-textarea.component'
import { FormNumberComponent } from './form-number/form-number.component'
import { FormSelectComponent } from './form-select/form-select.component'
import { FormFileComponent } from './form-file/form-file.component'
import { FormMultiselectComponent } from './form-multiselect/form-multiselect.component'
import { FormUnionComponent } from './form-union/form-union.component'
import { FormObjectComponent } from './form-object/form-object.component'
import { FormArrayComponent } from './form-array/form-array.component'
import { FormControlComponent } from './form-control/form-control.component'
import { MustachePipe } from './mustache.pipe'
import { ControlDirective } from './control.directive'
import { FormColorComponent } from './form-color/form-color.component'
import { MaskitoModule } from '@maskito/angular'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    TuiInputModule,
    TuiInputNumberModule,
    TuiInputFilesModule,
    TuiTextAreaModule,
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
    TuiSvgModule,
    TuiWrapperModule,
  ],
  declarations: [
    FormGroupComponent,
    FormControlComponent,
    FormColorComponent,
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
    ControlDirective,
  ],
  exports: [FormGroupComponent],
})
export class FormModule {}
