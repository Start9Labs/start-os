import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { ServerMetrics } from 'src/app/services/api/api.types'
import { DataComponent } from './data.component'
import { i18nKey } from '@start9labs/shared'

const LABELS: Record<string, i18nKey> = {
  percentageUsed: 'Percentage used',
  userSpace: 'User space',
  kernelSpace: 'Kernel space',
  idle: 'Idle',
  wait: 'I/O wait',
}

@Component({
  selector: 'metrics-cpu',
  template: `
    <div class="cpu">
      <div class="meter"></div>
      <div class="arrow" [style.transform]="transform()"></div>
      <div class="percent">{{ value()?.percentageUsed?.value }}%</div>
    </div>
    <metrics-data [labels]="labels" [value]="value()" />
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    .cpu {
      position: relative;
      margin: 1rem auto;
      width: 8rem;
      aspect-ratio: 1;
    }

    .meter {
      position: absolute;
      inset: 7%;
      mask: url(/assets/img/meter.svg);
      background: conic-gradient(
        from 180deg,
        var(--tui-status-positive) 30%,
        var(--tui-status-warning),
        var(--tui-status-negative) 70%
      );
    }

    .percent {
      position: absolute;
      bottom: 10%;
      width: 100%;
      text-align: center;
      font: var(--tui-font-text-l);
    }

    .arrow {
      @include taiga.transition(transform);
      position: absolute;
      top: 50%;
      left: 50%;

      &::before {
        content: '';
        position: absolute;
        inset: -0.5rem;
        background: var(--tui-background-base-alt);
        border-radius: 100%;
      }

      &::after {
        content: '';
        position: absolute;
        width: 0.25rem;
        height: 2rem;
        border-radius: 1rem;
        background: var(--tui-text-primary);
        transform: translate(-0.125rem, -0.125rem);
        clip-path: polygon(0 0, 100% 0, 60% 100%, 40% 100%);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [DataComponent],
})
export class CpuComponent {
  readonly value = input<ServerMetrics['cpu']>()

  readonly transform = computed(
    (value = this.value()?.percentageUsed?.value || '0') =>
      `rotate(${60 + (300 * Number.parseFloat(value)) / 100}deg)`,
  )

  readonly labels = LABELS
}
