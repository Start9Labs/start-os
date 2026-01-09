import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { WanIpv4Form, IPV4_LABELS, netmaskFromPrefix } from './utils'
import { DnsSummary } from '../../dns/summary'

export const SUMMARY = [
  'wan',
  'prefix',
  'mask',
  'gateway',
  'password',
  'device',
] as const

@Component({
  selector: '[ipv4Summary]',
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
export class Ipv4Summary {
  protected readonly service = injectFormService<WanIpv4Form>()

  readonly items = computed((ip = this.service.data()?.ip) =>
    ip
      ? SUMMARY.map(key => ({
          label: IPV4_LABELS[key].replace(/\*/g, ''),
          val: key === 'mask' ? netmaskFromPrefix(ip.prefix) : ip[key],
        }))
      : [],
  )
}
