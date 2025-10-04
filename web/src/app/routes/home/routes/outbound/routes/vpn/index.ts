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
import { Help } from 'src/app/directives/help.directive'

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
    <article vpnSummary tuiCardLarge="compact"></article>
    <form
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      tuiForm="m"
      [formGroup]="form"
    >
      <h3 tuiHeader="body-m">
        VPN Chaining
        <tui-icon tuiTooltip="Send traffic from this VPN through another VPN" />
        <aside tuiAccessories [style.margin-inline-start]="'auto'">
          <input type="checkbox" tuiSwitch formControlName="chaining" />
        </aside>
      </h3>
      <fieldset>
        <tui-textfield>
          <label tuiLabel>Label</label>
          <input tuiTextfield formControlName="label" />
        </tui-textfield>
        @if (form.value.chaining) {
          <tui-textfield tuiChevron>
            <label tuiLabel>VPN Label</label>
            <input tuiSelect formControlName="vpn" />
            <tui-data-list-wrapper
              *tuiTextfieldDropdown
              new
              [items]="['Proton', 'NordVPN']"
            />
          </tui-textfield>
        }
      </fieldset>
      <footer>
        <button
          tuiButton
          type="button"
          appearance="secondary-destructive"
          [style.margin-inline-end]="'auto'"
        >
          Delete
        </button>
        <button tuiButton type="button" appearance="flat" routerLink="..">
          Cancel
        </button>
        <button tuiButton>Save</button>
      </footer>
    </form>
  `,
  styles: `
    fieldset {
      grid-auto-flow: row;
      grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr));
    }
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
    Help,
    VPNAside,
    VPNSummary,
    TuiButton,
  ],
})
export default class OutboundVPN {
  public readonly form = inject(NonNullableFormBuilder).group({
    label: ['Mullvad', Validators.required],
    chaining: true,
    vpn: 'Proton',
  })
}
