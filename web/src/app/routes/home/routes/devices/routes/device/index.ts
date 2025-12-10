import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  TuiAppearance,
  TuiButton,
  TuiLink,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { DeviceAside } from './aside'
import { DeviceSummary } from './summary'

@Component({
  template: `
    <device-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [style.font]="'inherit'"
          >
            Manage Device
          </a>
        </h2>
      </hgroup>
    </header>
    <article deviceSummary [formLoading]="false"></article>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      class="g-form"
      [formGroup]="form"
    >
      <h3 tuiHeader><span tuiTitle>Name</span></h3>
      <tui-textfield>
        <input tuiTextfield formControlName="name" />
      </tui-textfield>
    </form>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      class="g-form"
      [formGroup]="form"
    >
      <h3 tuiHeader>
        <span tuiTitle>IP Addresses</span>
        <label tuiAccessories [style.font]="'var(--tui-font-text-m)'">
          Use static
          <input type="checkbox" tuiSwitch formControlName="static" />
        </label>
      </h3>
      <fieldset>
        <tui-textfield>
          <input
            tuiTextfield
            formControlName="ipv4"
            [readOnly]="!form.value.static"
          />
        </tui-textfield>
        <tui-textfield>
          <input
            tuiTextfield
            formControlName="ipv6"
            [readOnly]="!form.value.static"
          />
        </tui-textfield>
      </fieldset>
    </form>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      class="g-form"
      [formGroup]="form"
    >
      <h3 tuiHeader>
        <span tuiTitle>IPv6 Firewall</span>
        <label tuiAccessories [style.font]="'var(--tui-font-text-m)'">
          Allow all ports
          <input type="checkbox" tuiSwitch formControlName="firewall" />
        </label>
      </h3>
    </form>
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton>Save</button>
    </footer>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiCardLarge,
    TuiAppearance,
    TuiForm,
    TuiTextfield,
    TuiSwitch,
    TuiButton,
    Form,
    Help,
    DeviceAside,
    DeviceSummary,
  ],
})
export default class DevicesDevice {
  readonly form = inject(NonNullableFormBuilder).group({
    name: 'Pixel',
    static: false,
    ipv4: '127.0.0.1',
    ipv6: 'fe80::1ff:fe23:4567:890a',
    firewall: true,
  })
}
