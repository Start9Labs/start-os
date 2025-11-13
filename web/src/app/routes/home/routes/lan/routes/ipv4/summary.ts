import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary, SummaryItem } from 'src/app/components/summary'

import Ipv4 from '.'

@Component({
  selector: '[ipv4Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <label appSummary>
        DHCP Strategy
        <span tuiSubtitle>{{ parent.form.value.dhcp?.mode }}</span>
      </label>
      <label
        [appSummary]="
          parent.form.value.ip?.range + '.168.0.' + parent.form.value.ip?.router
        "
      >
        Router's IP Address
      </label>
      <label appSummary>
        DMZ
        <span tuiSubtitle>
          @if (parent.form.value.dmz) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </label>
    </section>
  `,
  styles: `
    span {
      text-transform: capitalize;
    }
  `,
  hostDirectives: [Summary],
  imports: [AsyncPipe, TuiHeader, TuiTitle, TuiBadge, TuiStatus, SummaryItem],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv4Summary {
  protected readonly parent = inject(Ipv4)
}
