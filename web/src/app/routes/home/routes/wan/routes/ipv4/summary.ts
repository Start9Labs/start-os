import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

import Ipv4 from '.'

@Component({
  selector: '[ipv4Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        IP Address Strategy
        <span tuiSubtitle>{{ parent.form.value.ip?.mode }}</span>
      </label>
      <label [appSummary]="parent.form.value.ip?.[parent.ip]?.wan">
        WAN IP Address
      </label>
      <label appSummary>
        Subnet Prefix
        <span tuiSubtitle>
          {{ $any(parent.form.value.ip)?.[parent.ip]?.prefix || '-' }}
        </span>
      </label>
      <label [appSummary]="$any(parent.form.value.ip)?.[parent.ip]?.mask">
        Subnet Mask
      </label>
      <label [appSummary]="$any(parent.form.value.ip)?.[parent.ip]?.gateway">
        Gateway IP Address
      </label>
      <label appSummary>
        DNS Strategy
        <span tuiSubtitle>{{ parent.form.value.dns?.mode }}</span>
      </label>
      <label appSummary>
        DNS Proxy
        <span tuiSubtitle>
          @if (parent.form.value.dns?.proxy) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </label>
    </section>
  `,
  hostDirectives: [Summary],
  imports: [AsyncPipe, TuiHeader, TuiTitle, TuiBadge, TuiStatus, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv4Summary {
  protected readonly parent = inject(Ipv4)
}
