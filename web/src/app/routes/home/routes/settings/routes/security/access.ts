import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiLabel, TuiNotification, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Security from '.'

@Component({
  selector: 'security-access',
  template: `
    <form tuiForm="m" tuiCardLarge class="g-form" [formGroup]="form">
      <header tuiHeader>
        <h2 tuiTitle>Remote Access</h2>
      </header>
      @if (form.value.remote === 'always') {
        <div tuiNotification appearance="warning">
          This setting is not recommended as your router will be exposed to the
          internet
        </div>
      }
      <section>
        @for (value of ['default', 'never', 'always']; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="remote"
              [value]="value"
            />
            {{ $index ? value : 'When behind NAT (Default)' }}
          </label>
        }
      </section>
    </form>
  `,
  styles: `
    label {
      text-transform: capitalize;
    }
  `,
  imports: [
    TuiCardLarge,
    TuiForm,
    TuiHeader,
    TuiTitle,
    ReactiveFormsModule,
    TuiNotification,
    TuiLabel,
    TuiRadio,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecurityAccess {
  protected readonly form = inject(Security).form
}
