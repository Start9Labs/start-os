import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { TuiProgress } from '@taiga-ui/kit'
import { ServerMetrics } from 'src/app/services/api/api.types'
import { DataComponent } from './data.component'

const LABELS = {
  percentageUsed: 'Percentage Used',
  capacity: 'Capacity',
  used: 'Used',
  available: 'Available',
}

@Component({
  standalone: true,
  selector: 'metrics-storage',
  template: `
    <progress
      tuiProgressBar
      [max]="100"
      [attr.value]="value()?.percentageUsed?.value"
    ></progress>
    <metrics-data [labels]="labels" [value]="value()" />
  `,
  styles: `
    progress {
      height: 1.5rem;
      width: 80%;
      margin: 3.75rem auto;
      border-radius: 0;
      clip-path: none;
      mask: linear-gradient(to right, #000 80%, transparent 80%);
      mask-size: 5% 100%;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiProgress, DataComponent],
})
export class StorageComponent {
  readonly value = input<ServerMetrics['disk']>()

  readonly used = computed(
    (
      capacity = this.value()?.capacity?.value,
      used = this.value()?.used?.value,
    ) =>
      capacity && used
        ? (
            (Number.parseFloat(used) / Number.parseFloat(capacity)) *
            100
          ).toFixed(1)
        : '-',
  )

  readonly labels = LABELS
}
