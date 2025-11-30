import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { SummaryItem } from 'src/app/components/summary'
import { Form } from 'src/app/directives/form.directive'

import Ipv4 from '.'
import { LABELS } from './utils'

@Component({
  selector: '[ipv4Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        IP Address Strategy
        <span tuiSubtitle>{{ labels[parent.form.value.ip?.mode || ''] }}</span>
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
        <span tuiSubtitle>{{ labels[parent.form.value.dns?.mode || ''] }}</span>
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
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  hostDirectives: [Form],
  imports: [AsyncPipe, TuiHeader, TuiTitle, TuiBadge, TuiStatus, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv4Summary {
  protected readonly parent = inject(Ipv4)
  protected readonly labels = LABELS
}
