import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { FormColorComponent } from './color.component'
import { FormDatetimeComponent } from './datetime.component'
import { FormFileComponent } from './file.component'
import { FormMultiselectComponent } from './multiselect.component'
import { FormNumberComponent } from './number.component'
import { FormSelectComponent } from './select.component'
import { FormTextComponent } from './text.component'
import { FormTextareaComponent } from './textarea.component'
import { FormToggleComponent } from './toggle.component'

export const CONTROLS = {
  color: new PolymorpheusComponent(FormColorComponent),
  datetime: new PolymorpheusComponent(FormDatetimeComponent),
  file: new PolymorpheusComponent(FormFileComponent),
  number: new PolymorpheusComponent(FormNumberComponent),
  select: new PolymorpheusComponent(FormSelectComponent),
  multiselect: new PolymorpheusComponent(FormMultiselectComponent),
  text: new PolymorpheusComponent(FormTextComponent),
  textarea: new PolymorpheusComponent(FormTextareaComponent),
  toggle: new PolymorpheusComponent(FormToggleComponent),
}
