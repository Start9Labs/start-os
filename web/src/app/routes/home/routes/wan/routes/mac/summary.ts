import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { injectFormService } from 'src/app/services/form.service'
import { MacForm, MAC_LABELS } from './utils'

@Component({
  selector: '[macSummary]',
  template: `
    <section>
      @if (mac(); as mac) {
        <div [appSummary]="mac">{{ labels.mac }}</div>
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MacSummary {
  protected readonly service = injectFormService<MacForm>()
  protected readonly labels = MAC_LABELS

  readonly mac = computed(() => this.service.data()?.address.mac)
}
