import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import {
  TuiAppearance,
  TuiButton,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help.directive'

import { MacAside } from './aside'
import { MacSummary } from './summary'

@Component({
  template: `
    <mac-aside *help />
    <article macSummary tuiCardLarge="compact"></article>
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader><h2 tuiTitle>Strategy</h2></header>
      <section>
        @for (value of ['router', 'custom']; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="strategy"
              [value]="value"
            />
            {{ value }}{{ $index ? '' : ' (Default)' }}
          </label>
        }
      </section>
      <section>
        <tui-textfield>
          <label tuiLabel>MAC Address*</label>
          <input
            tuiTextfield
            formControlName="mac"
            [readOnly]="form.value.strategy === 'router'"
          />
        </tui-textfield>
      </section>
    </form>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  styles: `
    [tuiLabel] {
      text-transform: capitalize;
    }
  `,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiCard,
    TuiRadio,
    TuiButton,
    MacSummary,
    MacAside,
    Help,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Mac {
  public readonly form = inject(NonNullableFormBuilder).group({
    strategy: 'router',
    mac: '94:83:C4:3B:D2:2B',
  })
}
