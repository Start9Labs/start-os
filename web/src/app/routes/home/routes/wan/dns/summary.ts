import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { Summary } from 'src/app/components/summary'
import { DnsForm, LABELS } from './utils'

@Component({
  selector: '[dnsSummary]',
  template: `
    <div appSummary>
      DNS Strategy
      <span tuiSubtitle>{{ dnsSummary().mode }}</span>
    </div>
  `,
  imports: [Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DnsSummary {
  readonly dnsSummary = input.required({
    transform: (dns?: DnsForm) => (dns ? { mode: LABELS[dns.mode] } : {}),
  })
}
