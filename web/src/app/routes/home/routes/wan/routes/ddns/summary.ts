import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

import Ddns from '.'

@Component({
  selector: '[ddnsSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        Status
        <span tuiSubtitle>
          @if (parent.form.value.dynamic) {
            <span tuiBadge tuiStatus appearance="positive">Connected</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </label>
      @if (parent.form.value.dynamic) {
        <label appSummary>
          Provider
          <span tuiSubtitle>{{ parent.form.value.provider }}</span>
        </label>
        <label appSummary="agf5d.start9.me">Hostname</label>
      }
    </section>
  `,
  hostDirectives: [Summary],
  imports: [AsyncPipe, TuiHeader, TuiBadge, TuiStatus, TuiTitle, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DdnsSummary {
  protected readonly parent = inject(Ddns)
}
