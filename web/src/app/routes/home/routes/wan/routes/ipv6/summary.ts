import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'

import Ipv6 from '.'

@Component({
  selector: '[ipv6Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <section>
      <div appSummary>
        IP Address Strategy
        <span tuiSubtitle>{{ parent.form.value.ip?.mode }}</span>
      </div>
      <div [appSummary]="$any(parent.form.value.ip)?.[parent.ip]?.wan">
        WAN IP Address
      </div>
      <div appSummary>
        Subnet Prefix
        <span tuiSubtitle>
          {{ $any(parent.form.value.ip)?.[parent.ip]?.prefix || '-' }}
        </span>
      </div>
      <div [appSummary]="$any(parent.form.value.ip)?.[parent.ip]?.mask">
        Subnet Mask
      </div>
      <div [appSummary]="$any(parent.form.value.ip)?.[parent.ip]?.gateway">
        Gateway IP Address
      </div>
      <div appSummary>
        DNS Strategy
        <span tuiSubtitle>{{ parent.form.value.dns?.mode }}</span>
      </div>
      <div appSummary>
        DNS Proxy
        <span tuiSubtitle>
          @if (parent.form.value.dns?.proxy) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [AsyncPipe, TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv6Summary {
  protected readonly parent = inject(Ipv6)
}
