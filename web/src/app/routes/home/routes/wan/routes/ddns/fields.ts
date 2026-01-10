import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import { DDNS_FIELD_LABELS, DDNS_VALIDATION_ERRORS } from './utils'

@Component({
  selector: 'ddns-fields',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Credentials</h2></header>
    <section>
      @for (field of fields(); track field) {
        <div>
          <tui-textfield>
            <label tuiLabel>{{ fieldLabels[field] }}*</label>
            <input
              tuiTextfield
              [formControlName]="field"
              [type]="
                field === 'password' || field === 'token' ? 'password' : 'text'
              "
            />
          </tui-textfield>
          <tui-error
            [formControlName]="field"
            [error]="[] | tuiFieldError | async"
          />
        </div>
      }
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: DDNS_VALIDATION_ERRORS,
    },
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
  ],
})
export class DdnsFields {
  protected readonly fieldLabels = DDNS_FIELD_LABELS

  readonly fields = input.required<string[]>()
}
