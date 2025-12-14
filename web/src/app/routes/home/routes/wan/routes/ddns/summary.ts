import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { Summary } from 'src/app/components/summary'

import Ddns from '.'

@Component({
  selector: '[ddnsSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <section>
      <div appSummary>
        Status
        <span tuiSubtitle>
          @if (parent.form.value.dynamic) {
            <span tuiBadge tuiStatus appearance="positive">Connected</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
      @if (parent.form.value.dynamic) {
        <div appSummary>
          Provider
          <span tuiSubtitle>{{ parent.form.value.provider }}</span>
        </div>
        <div appSummary="agf5d.start9.me">Hostname</div>
      }
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [AsyncPipe, TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DdnsSummary {
  protected readonly parent = inject(Ddns)
}
