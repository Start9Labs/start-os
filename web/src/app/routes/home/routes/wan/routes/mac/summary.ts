import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

import Mac from '.'

@Component({
  selector: '[macSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        Strategy
        <span tuiSubtitle>{{ parent.form.value.strategy }}</span>
      </label>
      <label [appSummary]="parent.form.value.mac">MAC Address</label>
    </section>
  `,
  hostDirectives: [Summary],
  imports: [AsyncPipe, TuiHeader, TuiTitle, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MacSummary {
  protected readonly parent = inject(Mac)
}
