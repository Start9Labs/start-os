import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

import Ipv6 from '.'

@Component({
  selector: '[ipv6Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        IPv6 Address Strategy
        <span tuiSubtitle [style.gap.rem]="0.375">
          @if (parent.form.value.slaac) {
            <span tuiBadge tuiStatus appearance="positive">SLAAC</span>
          }
          @if (parent.form.value.dhcpv6) {
            <span tuiBadge tuiStatus appearance="positive">DHCPv6</span>
          }
        </span>
      </label>
      <label [appSummary]="parent.form.value.ip">IPv6 Address</label>
      <label appSummary>
        IPv6 Prefix Length
        <span tuiSubtitle>/{{ parent.form.value.prefix }}</span>
      </label>
    </section>
  `,
  hostDirectives: [Summary],
  imports: [AsyncPipe, TuiHeader, TuiTitle, TuiBadge, TuiStatus, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv6Summary {
  protected readonly parent = inject(Ipv6)
}
