import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiStatus } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary } from 'src/app/components/summary'

import Ipv4 from '.'

@Component({
  selector: '[ipv4Summary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader><h2 tuiTitle>Summary</h2></header>
    <section>
      <div appSummary>
        DHCP Strategy
        <span tuiSubtitle>{{ parent.form.value.dhcp?.mode }}</span>
      </div>
      <div
        [appSummary]="
          parent.form.value.ip?.range + '.168.0.' + parent.form.value.ip?.router
        "
      >
        Router's IP Address
      </div>
      <div appSummary>
        DMZ
        <span tuiSubtitle>
          @if (parent.form.value.dmz) {
            <span tuiBadge tuiStatus appearance="positive">Enabled</span>
          } @else {
            <span tuiBadge tuiStatus appearance="neutral">Disabled</span>
          }
        </span>
      </div>
    </section>
  `,
  styles: `
    span {
      text-transform: capitalize;
    }
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [AsyncPipe, TuiHeader, TuiTitle, TuiBadge, TuiStatus, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class Ipv4Summary {
  protected readonly parent = inject(Ipv4)
}
