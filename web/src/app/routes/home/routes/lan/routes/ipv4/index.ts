import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiNonNullableValueTransformer } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  tuiNumberFormatProvider,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { tuiInputNumberOptionsProvider, TuiSwitch } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help.directive'

import { IPv4Aside } from './aside'
import { Ipv4Dhcp } from './dhcp'
import { Ipv4Ip } from './ip'
import { Ipv4Summary } from './summary'

@Component({
  template: `
    <ipv4-aside *help />
    <article ipv4Summary tuiCardLarge="compact"></article>
    <ipv4-dhcp />
    <ipv4-ip />
    <label
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader>
        <h2 tuiTitle>DMZ</h2>
        <aside tuiAccessories>
          <input type="checkbox" tuiSwitch size="m" formControlName="dmz" />
        </aside>
      </header>
    </label>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  providers: [
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
    tuiNumberFormatProvider({ precision: 0 }),
    tuiInputNumberOptionsProvider({
      min: 0,
      max: 255,
      valueTransformer: new TuiNonNullableValueTransformer(),
    }),
  ],
  imports: [
    TuiCard,
    TuiButton,
    TuiAppearance,
    TuiSwitch,
    TuiHeader,
    TuiTitle,
    ReactiveFormsModule,
    Help,
    Ipv4Summary,
    IPv4Aside,
    Ipv4Dhcp,
    Ipv4Ip,
    TuiForm,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Ipv4 {
  private readonly builder = inject(NonNullableFormBuilder)

  public readonly form = this.builder.group({
    dhcp: this.builder.group({
      mode: 'server',
      start: 2,
      end: 254,
    }),
    ip: this.builder.group({
      range: 192,
      router: 1,
    }),
    dmz: false,
  })
}
