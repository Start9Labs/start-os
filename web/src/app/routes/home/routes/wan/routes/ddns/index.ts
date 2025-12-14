import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { DdnsAside } from './aside'
import { DdnsSummary } from './summary'

@Component({
  template: `
    <ddns-aside *help />
    <header tuiHeader="h6"><h2 tuiTitle>Summary</h2></header>
    <article ddnsSummary [formLoading]="false"></article>
    <header tuiHeader="h6"><h2 tuiTitle>Settings</h2></header>
    <form [formGroup]="form" [formLoading]="false" [style.gap.rem]="1">
      <header tuiHeader="body-l"><h2 tuiTitle>Strategy</h2></header>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="dynamic" />
        Enable
      </label>
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
      <footer appFooter></footer>
    </form>
  `,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiSwitch,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    Form,
    Footer,
    Help,
    DdnsSummary,
    DdnsAside,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ddns {
  public readonly form = inject(NonNullableFormBuilder).group({
    dynamic: true,
    provider: 'Start9',
  })
}
