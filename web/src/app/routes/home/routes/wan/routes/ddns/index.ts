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

import { DdnsAside } from './aside'
import { DdnsSummary } from './summary'

@Component({
  template: `
    <ddns-aside *help />
    <article ddnsSummary tuiCardLarge="compact"></article>
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
            Enable
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
    DdnsSummary,
    DdnsAside,
    Help,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ddns {
  public readonly form = inject(NonNullableFormBuilder).group({
    dynamic: true,
    provider: 'Start9',
  })
}
