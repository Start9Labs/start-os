import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/components/form'
import { DEVICE_VALIDATION_ERRORS } from '../../utils'

@Component({
  selector: 'device-name',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Name</h2></header>
    <section>
      <div>
        <tui-textfield>
          <input tuiInput formControlName="name" [placeholder]="hostname()" />
        </tui-textfield>
        <tui-error formControlName="name" />
      </div>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(DEVICE_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiInput,
  ],
})
export class DeviceName {
  readonly hostname = input('')
}
