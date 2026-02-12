import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { WanIpv6Form, IPV6_LABELS } from './utils'

const SUMMARY_FIELDS = [
  'wan',
  'prefix',
  'gateway',
  'ip4',
  'mask',
  'border',
] as const

@Component({
  selector: '[wanIpv6Summary]',
  template: `
    <section>
      @for (item of items(); track item.label) {
        @if (item.val; as val) {
          <div [appSummary]="val">{{ item.label }}</div>
        }
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WanIpv6Summary {
  protected readonly service = injectFormService<WanIpv6Form>()

  readonly items = computed(() => {
    const ip = this.service.data()?.ip
    if (!ip) return []

    return SUMMARY_FIELDS.map(key => ({
      label: IPV6_LABELS[key],
      val: ip[key],
    }))
  })
}
