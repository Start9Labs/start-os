import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { buildNetworkBlock, buildRouterIp, LanIpv4Form } from './utils'

@Component({
  selector: '[lanIpv4Summary]',
  template: `
    <section>
      @if (networkBlock(); as block) {
        <div [appSummary]="block">Network Block</div>
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
    return data ? buildRouterIp(data.ip) : ''
  })

  readonly networkBlock = computed(() => {
    const data = this.service.data()
    return data ? buildNetworkBlock(data.ip) : ''
  })
}
