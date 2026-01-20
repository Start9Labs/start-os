import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ControlContainer, ReactiveFormsModule } from '@angular/forms'
import { TuiLabel, TuiNotification } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'

import Security from '../'

@Component({
  selector: 'security-access',
  template: `
    <section tuiForm="m" tuiCardLarge class="g-form">
      @if (form.value.remote === 'always') {
        <div tuiNotification appearance="warning">
          This setting is not recommended as your router will be exposed to the
          internet
        </div>
      }
      <div class="options">
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
      </div>
    </section>
  `,
  styles: `
    label {
      text-transform: capitalize;
    }

    .options {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
    }
  `,
  viewProviders: [
    {
      provide: ControlContainer,
      useFactory: () => inject(ControlContainer, { skipSelf: true }),
    },
  ],
  imports: [
    TuiCardLarge,
    TuiForm,
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
