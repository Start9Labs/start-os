import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiError, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TUI_VALIDATION_ERRORS, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { FORM, FormSection } from 'src/app/directives/form'
import { MAC_LABELS, MAC_VALIDATION_ERRORS } from './utils'

@Component({
  selector: 'mac-address',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Address</h2></header>
    <section>
      <div>
        <tui-textfield>
          <label tuiLabel>{{ labels.mac }}*</label>
          <input tuiTextfield formControlName="mac" />
        </tui-textfield>
        <tui-error formControlName="mac" [error]="[] | tuiFieldError | async" />
      </div>
    </section>
  `,
  viewProviders: [FORM],
  hostDirectives: [FormSection],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useValue: MAC_VALIDATION_ERRORS,
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
export class MacAddress {
  protected readonly labels = MAC_LABELS
}
