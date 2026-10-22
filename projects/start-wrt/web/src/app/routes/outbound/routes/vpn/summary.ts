import { Component, computed, inject } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'
import { buildChain } from 'src/app/routes/outbound/utils'
import OutboundVPN from 'src/app/routes/outbound/routes/vpn/index'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'
import { VPNChain } from './chain'

@Component({
  selector: '[vpnSummary]',
  template: `
    <section>
      <label appSummary>
        {{ 'Status' | i18n }}
        <span tuiSubtitle>
          @if (parent.data()?.enabled) {
            <span tuiBadge tuiStatus appearance="positive">
              {{ 'Connected' | i18n }}
            </span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">
              {{ 'Disabled' | i18n }}
            </span>
          }
        </span>
      </label>
      <label appSummary>
        {{ 'Used by' | i18n }}
        <span tuiSubtitle>{{ parent.data()?.used_by?.join(', ') || '-' }}</span>
      </label>
    </section>
    <section>
      <label appSummary>
        {{ 'Connection path' | i18n }}
        <vpn-chain tuiSubtitle [chain]="chain()" />
      </label>
    </section>
  `,
  styles: `
    label:has(vpn-chain) {
      flex: 1;
      min-inline-size: 0;
    }
  `,
  imports: [TuiBadge, TuiStatus, Summary, VPNChain, i18nPipe],
})
export class VPNSummary {
  protected readonly parent = inject(OutboundVPN)

  readonly chain = computed(() => {
    const label = this.parent.data()?.label
    return label ? buildChain(label, this.parent.service.data() ?? []) : []
  })
}
