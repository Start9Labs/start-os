import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { buildFullIp, LanIpv4Form } from './utils'

@Component({
  selector: '[lanIpv4Summary]',
  template: `
    <section>
      @if (subnet(); as subnet) {
        <div [appSummary]="subnet">Range</div>
      }
      @if (routerIp(); as ip) {
        <div [appSummary]="ip">Router IP</div>
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LanIpv4Summary {
  protected readonly service = injectFormService<LanIpv4Form>()

  readonly routerIp = computed(() => {
    const data = this.service.data()
    return data ? buildFullIp(data.ip) : ''
  })

  readonly subnet = computed(() => {
    const data = this.service.data()
    if (!data) return ''
    const ip = buildFullIp(data.ip)
    const parts = ip.split('.')
    return `${parts[0]}.${parts[1]}.${parts[2]}.0/24`
  })
}
