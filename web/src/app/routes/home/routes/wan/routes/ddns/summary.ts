import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { DdnsForm, DDNS_PROVIDERS } from './utils'

@Component({
  selector: '[ddnsSummary]',
  template: `
    <section>
      <div appSummary>
        Status
        <span tuiSubtitle>
          @if (enabled()) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
      @if (enabled()) {
        <div [appSummary]="providerLabel()">Provider</div>
        @if (hostname(); as hostname) {
          <div [appSummary]="hostname">Hostname</div>
        }
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DdnsSummary {
  protected readonly service = injectFormService<DdnsForm>()

  readonly enabled = computed(() => this.service.data()?.enabled ?? false)

  readonly providerLabel = computed(() => {
    const provider = this.service.data()?.provider
    return provider ? DDNS_PROVIDERS[provider].label : ''
  })

  readonly hostname = computed(() => this.service.data()?.hostname || '')
}
