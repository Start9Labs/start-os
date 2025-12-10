import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiRadio } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { MacAside } from './aside'
import { MacSummary } from './summary'

@Component({
  template: `
    <mac-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article macSummary [formLoading]="false"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form [formGroup]="form" [formLoading]="false" [style.gap.rem]="1">
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
      <footer appFooter></footer>
    </form>
  `,
  styles: `
    [tuiLabel] {
      text-transform: capitalize;
    }
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiRadio,
    Form,
    Footer,
    Help,
    MacSummary,
    MacAside,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Mac {
  public readonly form = inject(NonNullableFormBuilder).group({
    strategy: 'router',
    mac: '94:83:C4:3B:D2:2B',
  })
}
