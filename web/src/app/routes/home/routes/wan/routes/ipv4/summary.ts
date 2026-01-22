import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { WanIpv4Form, IPV4_LABELS, netmaskFromPrefix } from './utils'
import { DnsSummary } from '../../dns/summary'

const SUMMARY_FIELDS = [
  'wan',
  'prefix',
  'mask',
  'gateway',
  'username',
  'password',
  'device',
] as const

type SummaryField = (typeof SUMMARY_FIELDS)[number]

@Component({
  selector: '[wanIpv4Summary]',
  template: `
    <section>
      @for (item of items(); track item.label) {
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
export class WanIpv4Summary {
  protected readonly service = injectFormService<WanIpv4Form>()

  readonly items = computed(() => {
    const ip = this.service.data()?.ip
    if (!ip) return []

    return SUMMARY_FIELDS.map(key => ({
      label: IPV4_LABELS[key],
      val: this.getFieldValue(ip, key),
    }))
  })

  private getFieldValue(ip: WanIpv4Form['ip'], key: SummaryField): string {
    if (key === 'mask') {
      return netmaskFromPrefix(ip.prefix)
    }
    return ip[key as keyof typeof ip] as string
  }
}
