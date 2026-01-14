import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiLabel, TuiTitle } from '@taiga-ui/core'
import { TuiSwitch } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { FORM } from 'src/app/directives/form'
import LanIpv6 from '.'

@Component({
  selector: 'lan-ipv6-strategy',
  template: `
    <header tuiHeader="body-l"><h2 tuiTitle>Strategy</h2></header>
    <section>
      <label tuiLabel>
        <input type="checkbox" tuiSwitch size="m" formControlName="slaac" />
        Enable (SLAAC)
      </label>
      @if (parent.slaacEnabled()) {
        <label tuiLabel>
          <input type="checkbox" tuiSwitch size="m" formControlName="dhcpv6" />
          Also use DHCPv6
        </label>
      }
    </section>
  `,
  styles: `
    section {
      flex-direction: column !important;
      gap: 0.75rem !important;
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
