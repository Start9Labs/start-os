import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiLabel, TuiTitle } from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'

import LanIpv6 from '../'

@Component({
  selector: 'lan-ipv6-strategy',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Strategy</h2></header>
    <label tuiLabel>
      <input type="checkbox" tuiSwitch formControlName="slaac" />
      Enable (SLAAC)
    </label>
    @if (parent.slaacEnabled()) {
      <label tuiLabel>
        <input type="checkbox" tuiSwitch formControlName="dhcpv6" />
        Also use DHCPv6
      </label>
    }
  `,
  viewProviders: [FORM],
  hostDirectives: [TuiForm, TuiCardLarge],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ReactiveFormsModule, TuiHeader, TuiTitle, TuiLabel, TuiSwitch],
})
export class LanIpv6Strategy {
  protected readonly parent = inject(LanIpv6)
}
