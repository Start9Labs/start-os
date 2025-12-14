import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'

import { WanIpv4Form } from './types'
import { LABELS } from './utils'

@Component({
  selector: '[ipv4Summary]',
  template: `
    <section>
      <div appSummary>
        IP Address Strategy
        <span tuiSubtitle>{{ labels[ip()] }}</span>
      </div>
      <div [appSummary]="service.data()?.ip?.wan">WAN IP Address</div>
      <div appSummary>
        Subnet Prefix
        <span tuiSubtitle>{{ service.data()?.ip?.prefix || '-' }}</span>
      </div>
      <div [appSummary]="service.data()?.ip?.mask">Subnet Mask</div>
      <div [appSummary]="service.data()?.ip?.gateway">Gateway IP Address</div>
      <div appSummary>
        DNS Strategy
        <span tuiSubtitle>{{ labels[dns()] }}</span>
      </div>
      <div appSummary>
        DNS Proxy
        <span tuiSubtitle>
          @if (service.data()?.dns?.proxy) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv4Summary {
  protected readonly service = injectFormService<WanIpv4Form>()
  protected readonly labels = LABELS
  protected readonly ip = computed(() => this.service.data()?.ip.mode || 'dhcp')
  protected readonly dns = computed(
    () => this.service.data()?.dns.mode || 'isp',
  )
}
