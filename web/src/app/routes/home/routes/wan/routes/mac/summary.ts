import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Summary } from 'src/app/components/summary'

import Mac from '.'

@Component({
  selector: '[macSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <section>
      <div appSummary>
        Strategy
        <span tuiSubtitle>{{ parent.form.value.strategy }}</span>
      </div>
      <div [appSummary]="parent.form.value.mac">MAC Address</div>
    </section>
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [AsyncPipe, Summary],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MacSummary {
  protected readonly parent = inject(Mac)
}
