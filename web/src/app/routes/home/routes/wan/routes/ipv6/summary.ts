import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { WanIpv6Form, IPV6_LABELS } from './utils'
import { DnsSummary } from '../../dns/summary'

const SUMMARY = ['wan', 'prefix', 'gateway', 'ip4', 'mask', 'border'] as const

@Component({
  selector: '[ipv6Summary]',
  template: `
    <section>
      @for (item of items(); track $index) {
        @if (item.val; as val) {
          <div [appSummary]="val">{{ item.label }}</div>
        }
      }
      <div [dnsSummary]="service.data()?.dns"></div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary, DnsSummary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv6Summary {
  protected readonly service = injectFormService<WanIpv6Form>()

  readonly items = computed((ip = this.service.data()?.ip) =>
    ip
      ? SUMMARY.map(key => ({
          label: IPV6_LABELS[key].replace(/\*/g, ''),
          val: ip[key],
        }))
      : [],
  )
}
