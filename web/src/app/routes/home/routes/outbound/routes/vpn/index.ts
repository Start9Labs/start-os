import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { RouterLink } from '@angular/router'
import {
  TuiAppearance,
  TuiButton,
  TuiIcon,
  TuiLink,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
  TuiSwitch,
  TuiTooltip,
} from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { VPNAside } from './aside'
import { VPNSummary } from './summary'

@Component({
  template: `
    <vpn-aside *help />
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
            Manage Outbound VPN
          </a>
        </h2>
      </hgroup>
    </header>
    <article vpnSummary [formLoading]="false"></article>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      [formGroup]="form"
    >
      <h3 tuiHeader><span tuiTitle>Label</span></h3>
      <tui-textfield>
        <input tuiTextfield formControlName="label" />
      </tui-textfield>
    </form>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      [formGroup]="form"
      [style.margin-block.rem]="1"
    >
      <label tuiHeader [style.margin-inline]="0">
        <span tuiTitle>VPN Chaining</span>
        <aside tuiAccessories>
          <tui-icon
            tuiTooltip="Send traffic from this VPN through another VPN"
          />
          <input type="checkbox" tuiSwitch formControlName="chaining" />
        </aside>
      </label>
      @if (form.value.chaining) {
        <tui-textfield tuiChevron>
          <label tuiLabel>Chain to</label>
          <input tuiSelect formControlName="vpn" />
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="['Proton', 'NordVPN']"
          />
        </tui-textfield>
      }
    </form>
    <footer class="g-footer">
      <button
        tuiButton
        type="button"
        appearance="action-destructive"
        [style.margin-inline-end]="'auto'"
      >
        Delete
      </button>
      <button tuiButton type="button" appearance="flat" routerLink="..">
        Cancel
      </button>
      <button tuiButton>Save</button>
    </footer>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiForm,
    TuiCard,
    TuiTextfield,
    TuiSwitch,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiAppearance,
    TuiIcon,
    TuiTooltip,
    TuiButton,
    Form,
    Help,
    VPNAside,
    VPNSummary,
  ],
})
export default class OutboundVPN {
  public readonly form = inject(NonNullableFormBuilder).group({
    label: ['Mullvad', Validators.required],
    chaining: true,
    vpn: 'Proton',
  })
}
