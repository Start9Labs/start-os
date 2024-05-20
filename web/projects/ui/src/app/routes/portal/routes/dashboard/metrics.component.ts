import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiProgressModule } from '@taiga-ui/kit'
import { CpuComponent } from 'src/app/routes/portal/routes/dashboard/cpu.component'
import { MetricComponent } from 'src/app/routes/portal/routes/dashboard/metric.component'
import { TemperatureComponent } from 'src/app/routes/portal/routes/dashboard/temperature.component'
import { Metrics } from 'src/app/services/api/api.types'
import { TimeService } from 'src/app/services/time.service'

@Component({
  standalone: true,
  selector: 'app-metrics',
  template: `
    <ng-content />
    <section>
      <app-metric class="wide" label="Storage" [style.max-height.%]="85">
        <progress
          tuiProgressBar
          [max]="100"
          [attr.value]="metrics?.disk?.percentageUsed?.value"
        ></progress>
        <footer>
          <div>
            <span [attr.data-unit]="metrics?.disk?.used?.unit">
              {{ getValue(metrics?.disk?.used?.value) }}
            </span>
            Used
          </div>
          <hr />
          <div>
            <span [attr.data-unit]="metrics?.disk?.available?.unit">
              {{ getValue(metrics?.disk?.available?.value) }}
            </span>
            Available
          </div>
        </footer>
      </app-metric>
      <app-metric label="CPU">
        <app-cpu [value]="cpu" />
      </app-metric>
      <app-metric label="Memory">
        <label tuiProgressLabel>
          <tui-progress-circle size="l" [max]="100" [value]="memory" />
          {{ metrics?.memory?.percentageUsed?.value || ' - ' }}%
        </label>
        <footer>
          <div>
            <span [attr.data-unit]="metrics?.memory?.used?.unit">
              {{ getValue(metrics?.memory?.used?.value) }}
            </span>
            Used
          </div>
          <hr />
          <div>
            <span [attr.data-unit]="metrics?.memory?.available?.unit">
              {{ getValue(metrics?.memory?.available?.value) }}
            </span>
            Available
          </div>
        </footer>
      </app-metric>
      <aside>
        <app-metric label="Uptime" [style.flex]="'unset'">
          <label>
            {{ uptime() }}
            <div>Days : Hrs : Mins : Secs</div>
          </label>
        </app-metric>
        <app-metric label="Temperature">
          <app-temperature [value]="temperature" />
        </app-metric>
      </aside>
    </section>
  `,
  styles: `
    :host {
      grid-column: 1/3;

      --clip-path: polygon(
        0 2rem,
        1.25rem 0,
        8.75rem 0,
        calc(10rem + 0.1em) calc(2rem - 0.1em),
        11rem 2rem,
        calc(65% - 0.2em) 2rem,
        calc(65% + 1.25rem) 0,
        calc(100% - 1.25rem) 0,
        100% 2rem,
        100% calc(100% - 2rem),
        calc(100% - 1.25rem) 100%,
        10.5rem 100%,
        calc(9.25rem - 0.1em) calc(100% - 2rem + 0.1em),
        1.25rem calc(100% - 2rem),
        0 calc(100% - 4rem)
      );
    }

    section {
      height: 80%;
      display: flex;
      padding: 1rem 1.5rem 0.5rem;
      gap: 1rem;
    }

    aside {
      display: flex;
      flex: 1;
      flex-direction: column;
      gap: 1rem;
      margin-top: -1.5rem;
    }

    footer {
      display: flex;
      white-space: nowrap;
      background: var(--tui-clear);
    }

    label {
      margin: auto;
      text-align: center;
      padding: 0.375rem 0;
    }

    progress {
      height: 1.5rem;
      width: 80%;
      margin: auto;
      border-radius: 0;
      clip-path: none;
      mask: linear-gradient(to right, #000 80%, transparent 80%);
      mask-size: 5% 100%;
    }

    hr {
      height: 100%;
      width: 1px;
      margin: 0;
      background: rgba(0, 0, 0, 0.1);
    }

    div {
      display: flex;
      flex-direction: column;
      flex: 1;
      text-align: center;
      text-transform: uppercase;
      color: var(--tui-text-02);
      font-size: 0.5rem;
      line-height: 1rem;

      span {
        font-size: 0.75rem;
        font-weight: bold;
        color: var(--tui-text-01);
        padding-top: 0.4rem;

        &:after {
          content: attr(data-unit);
          font-size: 0.5rem;
          font-weight: normal;
          color: var(--tui-text-02);
        }
      }
    }

    :host-context(tui-root._mobile) {
      --clip-path: none !important;
      min-height: 100%;

      section {
        flex-wrap: wrap;
        padding: 0;
        margin: 1rem -1rem;
      }

      aside {
        order: -1;
        flex-direction: row;
        margin: 0;
      }

      app-metric {
        min-width: calc(50% - 0.5rem);

        &.wide {
          min-width: 100%;
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiProgressModule,
    MetricComponent,
    TemperatureComponent,
    CpuComponent,
    AsyncPipe,
  ],
})
export class MetricsComponent {
  @Input({ required: true })
  metrics: Metrics | null = null

  readonly uptime = toSignal(inject(TimeService).uptime$)

  get cpu(): number {
    return Number(this.metrics?.cpu.percentageUsed.value || 0) / 100
  }

  get temperature(): number {
    return Number(this.metrics?.general.temperature?.value || 0)
  }

  get memory(): number {
    return Number(this.metrics?.memory?.percentageUsed?.value) || 0
  }

  getValue(value?: string | null): number | string | undefined {
    return value == null ? '-' : Number.parseInt(value)
  }
}
