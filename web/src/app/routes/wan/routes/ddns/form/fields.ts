import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import {
  TuiError,
  TuiInput,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'
import { FORM } from 'src/app/components/form'
import { DDNS_FIELD_LABELS, DDNS_VALIDATION_ERRORS } from '../utils'

@Component({
  selector: 'ddns-fields',
  template: `
    <fieldset>
      <legend>Credentials</legend>
      <section>
        @for (field of fields(); track field) {
          <div>
            <tui-textfield>
              <label tuiLabel>{{ fieldLabels[field] }}</label>
              <input
                tuiInput
                [formControlName]="field"
                [type]="
                  ['password', 'token'].includes(field) ? 'password' : 'text'
                "
              />
            </tui-textfield>
            <tui-error [formControlName]="field" />
          </div>
        }
      </section>
    </fieldset>
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  providers: [tuiValidationErrorsProvider(DDNS_VALIDATION_ERRORS)],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ReactiveFormsModule, TuiTextfield, TuiError, TuiInput],
})
export class DdnsFields {
  protected readonly fieldLabels = DDNS_FIELD_LABELS

  readonly fields = input.required<string[]>()
}
