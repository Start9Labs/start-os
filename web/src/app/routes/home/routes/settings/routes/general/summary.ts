import { DatePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary } from 'src/app/components/summary'

@Component({
  selector: '[generalSummary]',
  template: `
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <div appSummary>
        Version
        <span tuiSubtitle>1.0.0</span>
      </div>
      <div appSummary>
        Language
        <span tuiSubtitle>English</span>
      </div>
      <div appSummary>
        Router Time
        <span tuiSubtitle>{{ date | date: 'full' }}</span>
      </div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [Summary, TuiHeader, TuiTitle, DatePipe],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GeneralSummary {
  protected readonly date = new Date()
}
