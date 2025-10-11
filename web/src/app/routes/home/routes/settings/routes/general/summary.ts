import { DatePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

@Component({
  selector: '[generalSummary]',
  template: `
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        Version
        <span tuiSubtitle>1.0.0</span>
      </label>
      <label appSummary>
        Language
        <span tuiSubtitle>English</span>
      </label>
      <label appSummary>
        Router Time
        <span tuiSubtitle>{{ date | date: 'full' }}</span>
      </label>
    </section>
  `,
  hostDirectives: [Summary],
  imports: [SummaryItem, TuiHeader, TuiTitle, DatePipe],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GeneralSummary {
  protected readonly date = new Date()
}
