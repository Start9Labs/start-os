import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import {
  TuiAppearance,
  TuiButton,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help.directive'

import { DnsAside } from './aside'
import { DnsSummary } from './summary'

@Component({
  template: `
    <dns-aside *help />
    <article dnsSummary tuiCardLarge="compact"></article>
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader>
        <h2 tuiTitle>Strategy</h2>
        <aside tuiAccessories>
          <label tuiLabel>
            <input type="checkbox" tuiSwitch formControlName="dynamic" />
            Use DNScrypt Proxy
          </label>
        </aside>
      </header>
      @if (form.value.dynamic) {
        <section>
          <tui-textfield tuiChevron [tuiTextfieldCleaner]="false">
            <label tuiLabel>Provider*</label>
            <input tuiSelect formControlName="provider" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="['Start9', 'Another']"
            />
          </tui-textfield>
        </section>
      }
    </form>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  imports: [
    ReactiveFormsModule,
    TuiForm,
    TuiAppearance,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiCard,
    TuiSwitch,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiButton,
    DnsSummary,
    DnsAside,
    Help,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Dns {
  public readonly form = inject(NonNullableFormBuilder).group({
    dynamic: true,
    provider: 'Start9',
  })
}
