import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'
import { MAC_LABELS, MAC_VALIDATION_ERRORS } from '../utils'

@Component({
  selector: 'mac-address',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Address</h2></header>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>{{ labels.mac }}*</label>
          <input tuiInput formControlName="mac" />
        </tui-textfield>
        <tui-error formControlName="mac" />
      </div>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(MAC_VALIDATION_ERRORS)],
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
export class MacAddress {
  protected readonly labels = MAC_LABELS
}
