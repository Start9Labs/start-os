import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import { DEVICE_VALIDATION_ERRORS } from '../../utils'

@Component({
  selector: 'device-name',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Name</h2></header>
    <section>
      <div>
        <tui-textfield>
          <input
            tuiTextfield
            formControlName="name"
            [placeholder]="hostname()"
          />
        </tui-textfield>
        <tui-error
          formControlName="name"
          [error]="[] | tuiFieldError | async"
        />
      </div>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    { provide: TUI_VALIDATION_ERRORS, useValue: DEVICE_VALIDATION_ERRORS },
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
export class DeviceName {
  readonly hostname = input('')
}
