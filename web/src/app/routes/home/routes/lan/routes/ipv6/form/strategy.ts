import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiHint, TuiLabel, TuiTitle } from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'

import LanIpv6 from '../'

@Component({
  selector: 'lan-ipv6-strategy',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Strategy</h2></header>
    <label
      tuiLabel
      [class.locked]="slaacLocked() && parent.slaacEnabled()"
      [tuiHint]="
        slaacLocked() && parent.slaacEnabled() ? slaacLockedReason() : null
      "
      tuiHintAppearance="error"
    >
      <input
        type="checkbox"
        tuiSwitch
        formControlName="slaac"
        (click)="onSlaacClick($event)"
      />
      Enable (SLAAC)
    </label>
    @if (parent.slaacEnabled()) {
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="dhcpv6" />
        Also use DHCPv6
      </label>
    }
  `,
  styles: `
    label.locked {
      opacity: 0.6;
      cursor: not-allowed;

      input {
        pointer-events: none;
      }
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLabel,
    TuiSwitch,
    TuiHint,
  ],
})
export class LanIpv6Strategy {
  protected readonly parent = inject(LanIpv6)

  readonly slaacLocked = input(false)
  readonly slaacLockedReason = input<string | null>(null)

  onSlaacClick(event: Event) {
    // Only prevent if trying to disable and it's locked
    if (this.slaacLocked() && this.parent.slaacEnabled()) {
      event.preventDefault()
      event.stopPropagation()
    }
  }
}
