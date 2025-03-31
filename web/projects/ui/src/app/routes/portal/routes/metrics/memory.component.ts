import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { TuiProgress } from '@taiga-ui/kit'
import { ServerMetrics } from 'src/app/services/api/api.types'
import { DataComponent } from './data.component'
import { ValuePipe } from './value.pipe'

const LABELS = {
  percentageUsed: 'Percentage Used',
  total: 'Total',
  used: 'Used',
  available: 'Available',
  zramUsed: 'zram Used',
  zramTotal: 'zram Total',
  zramAvailable: 'zram Available',
}

@Component({
  standalone: true,
  selector: 'metrics-memory',
  template: `
    <label tuiProgressLabel>
      <tui-progress-circle size="l" [max]="100" [value]="used()" />
      {{ value()?.percentageUsed?.value | value }}%
    </label>
    <metrics-data [labels]="labels" [value]="value()" />
  `,
  styles: `
    label {
      margin: 2rem auto;
      display: block;
      width: fit-content;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [DataComponent, TuiProgress, ValuePipe],
})
export class MemoryComponent {
  readonly value = input<ServerMetrics['memory']>()

  readonly used = computed(
    (value = this.value()?.percentageUsed.value || '0') =>
      Number.parseFloat(value),
  )

  readonly labels = LABELS
}
