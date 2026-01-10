import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { LanIpv6Data } from './uci/service'

@Component({
  selector: '[lanIpv6Summary]',
  template: `
    <section>
      <div appSummary>
        Strategy
        <span tuiSubtitle [style.gap.rem]="0.375">
          @if (slaac()) {
            <span tuiBadge tuiStatus appearance="positive">SLAAC</span>
          }
          @if (dhcpv6()) {
            <span tuiBadge tuiStatus appearance="positive">DHCPv6</span>
          }
          @if (!slaac()) {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
      @if (ip6addr(); as addr) {
        <div [appSummary]="addr">Router IP</div>
      }
      @if (prefix(); as p) {
        <div appSummary>
          Prefix Length
          <span tuiSubtitle>{{ p }}</span>
        </div>
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LanIpv6Summary {
  protected readonly service = injectFormService<LanIpv6Data>()

  readonly slaac = computed(() => this.service.data()?.strategy.slaac ?? false)
  readonly dhcpv6 = computed(
    () => this.service.data()?.strategy.dhcpv6 ?? false,
  )
  readonly prefix = computed(() =>
    this.service.data()?.strategy.slaac
      ? this.service.data()?.subnet.prefix
      : null,
  )
  readonly ip6addr = computed(() => this.service.data()?.ip6addr || '')
}
